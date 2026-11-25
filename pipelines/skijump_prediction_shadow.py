"""Run the ski-jumping prediction model in a publication-disabled workspace."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
from collections.abc import Callable, Iterable
from datetime import date, datetime
from pathlib import Path

from openpyxl import load_workbook

from pipelines.skijump_prediction_contract import CATEGORY_CONTRACTS, build_plan

SCHEMA_VERSION = "nordicnumbers.skijump-prediction-shadow/v1"
SAFE_RUN_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
INDIVIDUAL_COLUMNS = (
    "Skier",
    "ID",
    "Nation",
    "Sex",
    "Participation",
    "Win",
    "Podium",
    "Top-5",
    "Top-10",
    "Top-30",
)
TEAM_COLUMNS = (
    "Nation",
    "Sex",
    "RaceType",
    "Win",
    "Podium",
    "Top-5",
    "Top-10",
    "Top-30",
)
STARTLISTS = {
    "men": "elo/python/skijump/polars/excel365/startlist_races_men.csv",
    "ladies": "elo/python/skijump/polars/excel365/startlist_races_ladies.csv",
    "men_team": "elo/python/skijump/polars/relay/excel365/startlist_team_races_men.csv",
    "ladies_team": "elo/python/skijump/polars/relay/excel365/startlist_team_races_ladies.csv",
    "mixed_team": "elo/python/skijump/polars/relay/excel365/startlist_mixed_team_races_teams.csv",
}
STARTLIST_RELATIVES = frozenset(STARTLISTS.values())
NORMALIZATION_SCHEMA = "nordicnumbers.skijump-startlist-normalization/v1"


class PredictionShadowError(RuntimeError):
    """Raised when a shadow prediction cannot continue safely."""


def write_json_atomic(path: Path, document: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    temporary.replace(path)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def parse_date(value: str) -> date:
    try:
        return date.fromisoformat(value)
    except ValueError as error:
        raise PredictionShadowError("logical date must use YYYY-MM-DD") from error


def parse_source_date(value: str) -> date | None:
    value = value.strip()
    for pattern in ("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value, pattern).date()
        except ValueError:
            pass
    return None


def validate_startlist(path: Path, logical_date: date, category: str) -> int:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        if "Race_Date" not in (reader.fieldnames or ()):
            raise PredictionShadowError(
                f"{category} startlist is missing Race_Date: {path}"
            )
        rows = list(reader)
    matching = [
        row
        for row in rows
        if parse_source_date(row.get("Race_Date", "")) == logical_date
    ]
    if not matching:
        observed = sorted({row.get("Race_Date", "").strip() for row in rows})
        raise PredictionShadowError(
            f"{category} startlist does not match {logical_date}: observed={observed}"
        )
    return len(matching)


def validate_normalized_startlists(
    *,
    report_path: Path,
    candidate_root: Path,
    logical_date: date,
    active_categories: set[str],
) -> list[dict[str, object]]:
    """Verify the ski repository's normalization handoff before simulation."""
    try:
        report = json.loads(report_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise PredictionShadowError(
            f"cannot read startlist normalization report: {report_path}"
        ) from error
    expected_header = {
        "schema_version": NORMALIZATION_SCHEMA,
        "logical_date": logical_date.isoformat(),
        "status": "ready",
        "prediction_eligible": True,
        "accepted_startlists_modified": False,
    }
    observed_header = {key: report.get(key) for key in expected_header}
    if observed_header != expected_header:
        raise PredictionShadowError(
            f"startlist normalization is not prediction eligible: {observed_header}"
        )
    outputs = report.get("outputs")
    if not isinstance(outputs, dict) or set(outputs) != active_categories:
        raise PredictionShadowError(
            "normalized startlist categories do not match the execution plan: "
            f"observed={sorted(outputs) if isinstance(outputs, dict) else outputs!r} "
            f"expected={sorted(active_categories)}"
        )

    candidate_root = candidate_root.resolve()
    records: list[dict[str, object]] = []
    for category in sorted(active_categories):
        metadata = outputs.get(category)
        if not isinstance(metadata, dict):
            raise PredictionShadowError(
                f"invalid normalization metadata for {category}: {metadata!r}"
            )
        filename = Path(STARTLISTS[category]).name
        candidate = (candidate_root / filename).resolve()
        reported = (report_path.parent / str(metadata.get("file", ""))).resolve()
        if candidate.parent != candidate_root or reported != candidate:
            raise PredictionShadowError(
                f"normalization report points outside the candidate root for {category}"
            )
        if not candidate.is_file() or candidate.stat().st_size == 0:
            raise PredictionShadowError(
                f"normalized startlist is missing or empty: {candidate}"
            )
        observed_sha = sha256(candidate)
        if metadata.get("sha256") != observed_sha:
            raise PredictionShadowError(
                f"normalized startlist checksum mismatch for {category}"
            )
        rows = validate_startlist(candidate, logical_date, category)
        if metadata.get("rows") != rows:
            raise PredictionShadowError(
                f"normalized startlist row count mismatch for {category}: "
                f"report={metadata.get('rows')!r} observed={rows}"
            )
        records.append(
            {
                "category": category,
                "file": str(candidate),
                "bytes": candidate.stat().st_size,
                "rows": rows,
                "sha256": observed_sha,
                "source": "normalized_official_fis",
            }
        )
    return records


def materialize_model_inputs(
    *,
    run_root: Path,
    ski_root: Path,
    candidate_root: Path,
    active_categories: set[str],
) -> tuple[Path, Path]:
    """Build isolated model roots from accepted histories and generated lists."""
    individual_root = run_root / "model-inputs/individual"
    team_root = run_root / "model-inputs/team"
    individual_root.mkdir(parents=True)
    team_root.mkdir(parents=True)

    individual_source = ski_root / "elo/python/skijump/polars/excel365"
    team_source = ski_root / "elo/python/skijump/polars/relay/excel365"
    for filename in (
        "races.csv",
        "men_chrono_elevation.csv",
        "ladies_chrono_elevation.csv",
    ):
        source = individual_source / filename
        if source.is_file():
            shutil.copy2(source, individual_root / filename)
    for filename in (
        "men_team_chrono.csv",
        "ladies_team_chrono.csv",
        "mixed_team_chrono.csv",
    ):
        source = team_source / filename
        if source.is_file():
            shutil.copy2(source, team_root / filename)

    for category in active_categories:
        filename = Path(STARTLISTS[category]).name
        destination_root = team_root if category.endswith("_team") else individual_root
        shutil.copy2(candidate_root / filename, destination_root / filename)
    return individual_root, team_root


def expected_sheets(category: str, count: int) -> list[str]:
    prefix = str(CATEGORY_CONTRACTS[category]["sheet_prefix"])
    if category.endswith("_team"):
        return [prefix]
    return [f"{prefix} {index}" for index in range(1, count + 1)]


def validate_workbook(path: Path, category: str, count: int) -> dict[str, object]:
    workbook = load_workbook(path, read_only=True, data_only=True)
    wanted_sheets = expected_sheets(category, count)
    if workbook.sheetnames != wanted_sheets:
        raise PredictionShadowError(
            f"unexpected sheets in {path.name}: {workbook.sheetnames}; "
            f"expected {wanted_sheets}"
        )
    expected_columns = (
        TEAM_COLUMNS if category.endswith("_team") else INDIVIDUAL_COLUMNS
    )
    row_counts: dict[str, int] = {}
    probability_columns = {"Win", "Podium", "Top-5", "Top-10", "Top-30"}
    for sheet_name in wanted_sheets:
        sheet = workbook[sheet_name]
        # openxlsx can emit an underspecified A1 worksheet dimension even when
        # the sheet contains a full table. Recalculate it before streaming.
        if hasattr(sheet, "reset_dimensions"):
            sheet.reset_dimensions()
        rows = sheet.iter_rows(values_only=True)
        header = tuple(next(rows, ()))
        if header != expected_columns:
            raise PredictionShadowError(
                f"unexpected columns in {path.name}/{sheet_name}: {header}"
            )
        count_rows = 0
        win_total = 0.0
        for row in rows:
            if all(value is None for value in row):
                continue
            count_rows += 1
            values = dict(zip(header, row, strict=True))
            for column, value in zip(header, row, strict=True):
                if column in probability_columns and (
                    not isinstance(value, (int, float)) or not 0 <= value <= 100
                ):
                    raise PredictionShadowError(
                        f"invalid {column} probability in {path.name}/{sheet_name}: {value!r}"
                    )
            ordered = [
                values[column]
                for column in ("Win", "Podium", "Top-5", "Top-10", "Top-30")
            ]
            if ordered != sorted(ordered):
                raise PredictionShadowError(
                    f"non-monotonic probabilities in {path.name}/{sheet_name}: {ordered}"
                )
            win_total += float(values["Win"])
        if count_rows == 0:
            raise PredictionShadowError(
                f"empty prediction sheet: {path.name}/{sheet_name}"
            )
        if not 95 <= win_total <= 105:
            raise PredictionShadowError(
                f"winner probabilities do not sum to 100 in {path.name}/{sheet_name}: {win_total}"
            )
        row_counts[sheet_name] = count_rows
    workbook.close()
    return {"file": path.name, "sha256": sha256(path), "sheets": row_counts}


def run_shadow(
    *,
    blog_root: Path,
    ski_root: Path,
    output_root: Path,
    logical_date: str,
    run_id: str,
    n_simulations: int,
    random_seed: int,
    blog_commit: str,
    ski_commit: str,
    startlist_root: Path | None = None,
    startlist_report: Path | None = None,
    rscript: str = "Rscript",
    command_runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
) -> Path:
    target_date = parse_date(logical_date)
    if not SAFE_RUN_ID.fullmatch(run_id):
        raise PredictionShadowError(f"unsafe run ID: {run_id}")
    if n_simulations < 1:
        raise PredictionShadowError("n_simulations must be positive")
    for label, revision in (("blog", blog_commit), ("ski", ski_commit)):
        if not re.fullmatch(r"[0-9a-f]{40}", revision):
            raise PredictionShadowError(f"{label} commit must be a full SHA")

    blog_root = blog_root.expanduser().resolve()
    ski_root = ski_root.expanduser().resolve()
    output_root = output_root.expanduser().resolve()
    for repository in (blog_root, ski_root):
        if output_root == repository or repository in output_root.parents:
            raise PredictionShadowError("output root must be outside both repositories")

    schedule = ski_root / "elo/python/skijump/polars/excel365/races.csv"
    model = blog_root / "content/post/skijump/drafts/race-picks-simulation.R"
    if not schedule.is_file() or not model.is_file():
        raise PredictionShadowError("schedule or prediction model is missing")

    run_root = output_root / "runs" / run_id
    if run_root.exists():
        raise PredictionShadowError(f"shadow run already exists: {run_root}")
    reports_root = run_root / "reports"
    model_root = run_root / "workbooks"
    logs_root = run_root / "logs"
    report_path = reports_root / "shadow-report.json"
    plan_path = reports_root / "plan.json"
    run_root.mkdir(parents=True)

    report: dict[str, object] = {
        "schema_version": SCHEMA_VERSION,
        "pipeline_id": "skijump_prediction_shadow",
        "run_id": run_id,
        "logical_date": logical_date,
        "status": "failed",
        "code": {"blog_commit": blog_commit, "ski_commit": ski_commit},
        "parameters": {
            "n_simulations": n_simulations,
            "random_seed": random_seed,
        },
        "publication": {
            "status": "disabled",
            "reason": "shadow runs retain evidence and cannot create or publish posts",
        },
    }
    try:
        plan = build_plan(schedule, target_date)
        write_json_atomic(plan_path, plan)
        report["plan"] = str(plan_path.relative_to(run_root))
        if plan["status"] == "no_races":
            report["status"] = "skipped"
            report["reason"] = "no ski-jumping races are scheduled for the logical date"
            write_json_atomic(report_path, report)
            return report_path
        if plan["status"] != "ready_for_shadow_implementation":
            raise PredictionShadowError(
                f"prediction plan is blocked: {plan['blockers']}"
            )

        if (startlist_root is None) != (startlist_report is None):
            raise PredictionShadowError(
                "startlist root and normalization report must be provided together"
            )
        generated_startlists = startlist_root is not None
        inputs: list[dict[str, object]] = []
        for relative in plan["required_ski_inputs"]:
            if generated_startlists and relative in STARTLIST_RELATIVES:
                continue
            path = ski_root / str(relative)
            if not path.is_file() or path.stat().st_size == 0:
                raise PredictionShadowError(
                    f"required input is missing or empty: {relative}"
                )
            inputs.append(
                {
                    "file": str(relative),
                    "bytes": path.stat().st_size,
                    "sha256": sha256(path),
                }
            )
        active_categories = {
            category for category, count in plan["categories"].items() if count
        }
        if generated_startlists:
            assert startlist_root is not None and startlist_report is not None
            startlist_root = startlist_root.expanduser().resolve()
            startlist_report = startlist_report.expanduser().resolve()
            generated_inputs = validate_normalized_startlists(
                report_path=startlist_report,
                candidate_root=startlist_root,
                logical_date=target_date,
                active_categories=active_categories,
            )
            inputs.extend(generated_inputs)
            startlist_rows = {
                str(record["category"]): int(record["rows"])
                for record in generated_inputs
            }
            individual_input_root, team_input_root = materialize_model_inputs(
                run_root=run_root,
                ski_root=ski_root,
                candidate_root=startlist_root,
                active_categories=active_categories,
            )
            report["startlist_source"] = "normalized_official_fis"
            report["startlist_normalization_report"] = {
                "file": str(startlist_report),
                "sha256": sha256(startlist_report),
            }
        else:
            startlist_rows = {
                category: validate_startlist(
                    ski_root / STARTLISTS[category], target_date, category
                )
                for category in active_categories
            }
            individual_input_root = ski_root / "elo/python/skijump/polars/excel365"
            team_input_root = ski_root / "elo/python/skijump/polars/relay/excel365"
            report["startlist_source"] = "accepted_repository"
        report["inputs"] = inputs
        report["startlist_rows"] = startlist_rows

        environment = {
            **os.environ,
            "NORDIC_INPUT_ROOT": str(individual_input_root),
            "NORDIC_TEAM_INPUT_ROOT": str(team_input_root),
            "NORDIC_OUTPUT_ROOT": str(model_root),
            "NORDIC_BLOG_ROOT": str(blog_root),
            "NORDIC_LOG_ROOT": str(logs_root),
            "NORDIC_LOGICAL_DATE": logical_date,
            "NORDIC_RUN_ID": run_id,
            "NORDIC_RANDOM_SEED": str(random_seed),
            "NORDIC_N_SIMULATIONS": str(n_simulations),
            "NORDIC_LOAD_ENV": "false",
            "NORDIC_ENHANCED_LOGGING": "false",
            "TEST_MODE": "false",
        }
        logs_root.mkdir(parents=True, exist_ok=True)
        process_log = logs_root / "simulation-process.log"
        report["evidence"] = {
            "process_log": str(process_log.relative_to(run_root)),
            "simulation_log": str((logs_root / "simulation.log").relative_to(run_root)),
        }
        with process_log.open("w", encoding="utf-8") as handle:
            result = command_runner(
                [rscript, str(model)],
                cwd=blog_root,
                env=environment,
                text=True,
                stdout=handle,
                stderr=subprocess.STDOUT,
                check=False,
            )
        if result.returncode != 0:
            raise PredictionShadowError(
                f"R simulation failed with exit code {result.returncode}"
            )

        dated_root = model_root / plan["date_key"]
        expected = {
            str(CATEGORY_CONTRACTS[category]["workbook"]): (category, count)
            for category, count in plan["categories"].items()
            if count
        }
        observed = {path.name for path in dated_root.glob("*.xlsx")}
        if observed != set(expected):
            raise PredictionShadowError(
                f"workbook set mismatch: observed={sorted(observed)} "
                f"expected={sorted(expected)}"
            )
        outputs = [
            validate_workbook(dated_root / filename, category, count)
            for filename, (category, count) in sorted(expected.items())
        ]
        report["outputs"] = outputs
        report["status"] = "passed"
        write_json_atomic(report_path, report)
        return report_path
    except Exception as error:
        report["error"] = str(error)
        write_json_atomic(report_path, report)
        raise


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--blog-root", required=True, type=Path)
    parser.add_argument("--ski-root", required=True, type=Path)
    parser.add_argument("--output-root", required=True, type=Path)
    parser.add_argument("--logical-date", required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--n-simulations", required=True, type=int)
    parser.add_argument("--random-seed", required=True, type=int)
    parser.add_argument("--blog-commit", required=True)
    parser.add_argument("--ski-commit", required=True)
    parser.add_argument("--startlist-root", type=Path)
    parser.add_argument("--startlist-report", type=Path)
    parser.add_argument("--rscript", default="Rscript")
    return parser.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        report = run_shadow(
            blog_root=args.blog_root,
            ski_root=args.ski_root,
            output_root=args.output_root,
            logical_date=args.logical_date,
            run_id=args.run_id,
            n_simulations=args.n_simulations,
            random_seed=args.random_seed,
            blog_commit=args.blog_commit,
            ski_commit=args.ski_commit,
            startlist_root=args.startlist_root,
            startlist_report=args.startlist_report,
            rscript=args.rscript,
        )
    except Exception as error:  # noqa: BLE001 - CLI must surface any runner failure
        print(f"ski-jumping prediction shadow failed: {error}", file=sys.stderr)
        return 1
    print(report)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
