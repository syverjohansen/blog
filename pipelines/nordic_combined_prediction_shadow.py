"""Run Nordic Combined predictions in an isolated, publication-disabled workspace."""

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

from pipelines.nordic_combined_prediction_contract import CATEGORY_CONTRACTS, build_plan


SCHEMA_VERSION = "nordicnumbers.nordic-combined-prediction-shadow/v1"
SAFE_RUN_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
INDIVIDUAL_COLUMNS = (
    "Skier", "ID", "Nation", "Sex", "Participation", "Win", "Podium",
    "Top-5", "Top-10", "Top-30",
)
TEAM_COLUMNS = (
    "Nation", "Sex", "RaceType", "Win", "Podium", "Top-5", "Top-10", "Top-30",
)
PROBABILITY_COLUMNS = ("Win", "Podium", "Top-5", "Top-10", "Top-30")


class PredictionShadowError(RuntimeError):
    """Raised when the shadow cannot continue without weakening its evidence."""


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
    for pattern in ("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), pattern).date()
        except ValueError:
            pass
    return None


def _is_team(category: str) -> bool:
    return "_team" in category


def _expected_sex(category: str) -> str:
    if category.startswith("men"):
        return "M"
    if category.startswith("ladies"):
        return "L"
    return "Mixed"


def _expected_race_type(category: str) -> str | None:
    if not _is_team(category):
        return None
    if category == "mixed_team":
        return "Mixed Team"
    if category == "mixed_team_sprint":
        return "Mixed Team Sprint"
    return "Team Sprint" if category.endswith("team_sprint") else "Team"


def _startlist_relative(category: str) -> str:
    candidates = [
        str(path)
        for path in CATEGORY_CONTRACTS[category]["inputs"]
        if "startlist" in Path(str(path)).name
    ]
    if len(candidates) != 1:
        raise PredictionShadowError(
            f"{category} must declare exactly one startlist, observed={candidates}"
        )
    return candidates[0]


def _canonical_id(value: object) -> str:
    if isinstance(value, bool) or value is None:
        return ""
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return str(value).strip()


def read_startlist_contract(
    path: Path, *, category: str, count: int, logical_date: date
) -> list[set[str]]:
    """Return the exact expected workbook identities for each planned sheet."""
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        fields = set(reader.fieldnames or ())
        required = {"Nation"} if _is_team(category) else {"ID", "Skier", "Nation"}
        missing = sorted(required - fields)
        if missing:
            raise PredictionShadowError(
                f"{category} startlist is missing required columns {missing}: {path}"
            )
        rows = list(reader)
    if not rows:
        raise PredictionShadowError(f"{category} startlist is empty: {path}")

    if "Race_Date" in fields:
        dates = {parse_source_date(row.get("Race_Date", "")) for row in rows}
        if dates != {logical_date}:
            observed = sorted(str(value) for value in dates)
            raise PredictionShadowError(
                f"{category} startlist does not exclusively match {logical_date}: "
                f"observed={observed}"
            )

    if _is_team(category):
        identities = {row["Nation"].strip() for row in rows if row["Nation"].strip()}
        if len(identities) != len(rows):
            raise PredictionShadowError(
                f"{category} startlist has blank or duplicate Nation values"
            )
        return [identities]

    expected: list[set[str]] = []
    for index in range(1, count + 1):
        probability_columns = (f"Race{index}_Prob", f"Race_Prob{index}")
        selected = next((column for column in probability_columns if column in fields), None)
        active_rows = rows
        if selected is not None:
            active_rows = []
            for row in rows:
                try:
                    included = float(row.get(selected, "") or 0) > 0
                except ValueError as error:
                    raise PredictionShadowError(
                        f"invalid {selected} value in {category} startlist"
                    ) from error
                if included:
                    active_rows.append(row)
        identities = {_canonical_id(row.get("ID")) for row in active_rows}
        if "" in identities or len(identities) != len(active_rows):
            raise PredictionShadowError(
                f"{category} startlist race {index} has blank or duplicate ID values"
            )
        if not identities:
            raise PredictionShadowError(
                f"{category} startlist has no entrants for planned race {index}"
            )
        expected.append(identities)
    return expected


def materialize_inputs(
    *, run_root: Path, ski_root: Path, required_inputs: Iterable[str]
) -> tuple[Path, Path, list[dict[str, object]]]:
    """Copy only declared inputs into immutable run-local model roots."""
    individual_root = run_root / "model-inputs/individual"
    team_root = run_root / "model-inputs/team"
    individual_root.mkdir(parents=True)
    team_root.mkdir(parents=True)
    records: list[dict[str, object]] = []
    for relative in sorted(required_inputs):
        source = ski_root / relative
        if not source.is_file() or source.stat().st_size == 0:
            raise PredictionShadowError(f"required input is missing or empty: {relative}")
        destination_root = team_root if "/relay/excel365/" in relative else individual_root
        destination = destination_root / source.name
        if destination.exists() and destination.read_bytes() != source.read_bytes():
            raise PredictionShadowError(
                f"input filename collision while materializing {relative}"
            )
        shutil.copy2(source, destination)
        records.append(
            {
                "file": relative,
                "bytes": source.stat().st_size,
                "sha256": sha256(source),
                "materialized_as": str(destination.relative_to(run_root)),
            }
        )
    return individual_root, team_root, records


def expected_sheets(category: str, count: int) -> list[str]:
    prefix = str(CATEGORY_CONTRACTS[category]["sheet_prefix"])
    return [prefix] if _is_team(category) else [f"{prefix} {i}" for i in range(1, count + 1)]


def validate_workbook(
    path: Path, *, category: str, count: int, expected_identities: list[set[str]]
) -> dict[str, object]:
    workbook = load_workbook(path, read_only=True, data_only=True)
    wanted_sheets = expected_sheets(category, count)
    if workbook.sheetnames != wanted_sheets:
        workbook.close()
        raise PredictionShadowError(
            f"unexpected sheets in {path.name}: {workbook.sheetnames}; expected={wanted_sheets}"
        )
    columns = TEAM_COLUMNS if _is_team(category) else INDIVIDUAL_COLUMNS
    identity_column = "Nation" if _is_team(category) else "ID"
    sheet_records: dict[str, object] = {}
    for index, sheet_name in enumerate(wanted_sheets):
        sheet = workbook[sheet_name]
        if hasattr(sheet, "reset_dimensions"):
            sheet.reset_dimensions()
        rows = sheet.iter_rows(values_only=True)
        header = tuple(next(rows, ()))
        if header != columns:
            workbook.close()
            raise PredictionShadowError(
                f"unexpected columns in {path.name}/{sheet_name}: {header}"
            )
        observed_ids: set[str] = set()
        win_total = 0.0
        row_count = 0
        for row in rows:
            if all(value is None for value in row):
                continue
            row_count += 1
            values = dict(zip(header, row, strict=True))
            identity = _canonical_id(values[identity_column])
            if not identity or identity in observed_ids:
                workbook.close()
                raise PredictionShadowError(
                    f"blank or duplicate {identity_column} in {path.name}/{sheet_name}"
                )
            observed_ids.add(identity)
            if values["Sex"] != _expected_sex(category):
                workbook.close()
                raise PredictionShadowError(
                    f"unexpected Sex in {path.name}/{sheet_name}: {values['Sex']!r}"
                )
            expected_race_type = _expected_race_type(category)
            if expected_race_type is not None and values["RaceType"] != expected_race_type:
                workbook.close()
                raise PredictionShadowError(
                    f"unexpected RaceType in {path.name}/{sheet_name}: "
                    f"{values['RaceType']!r}"
                )
            probabilities = []
            for column in PROBABILITY_COLUMNS:
                value = values[column]
                if not isinstance(value, (int, float)) or isinstance(value, bool) or not 0 <= value <= 100:
                    workbook.close()
                    raise PredictionShadowError(
                        f"invalid {column} in {path.name}/{sheet_name}: {value!r}"
                    )
                probabilities.append(float(value))
            if probabilities != sorted(probabilities):
                workbook.close()
                raise PredictionShadowError(
                    f"non-monotonic probabilities in {path.name}/{sheet_name}: {probabilities}"
                )
            win_total += probabilities[0]
        if observed_ids != expected_identities[index]:
            workbook.close()
            raise PredictionShadowError(
                f"prediction identities differ from {category} startlist for {sheet_name}: "
                f"missing={sorted(expected_identities[index] - observed_ids)} "
                f"unexpected={sorted(observed_ids - expected_identities[index])}"
            )
        if not 95 <= win_total <= 105:
            workbook.close()
            raise PredictionShadowError(
                f"winner probabilities do not sum to 100 in {path.name}/{sheet_name}: {win_total}"
            )
        sheet_records[sheet_name] = {"rows": row_count, "win_total": win_total}
    workbook.close()
    return {"file": path.name, "sha256": sha256(path), "sheets": sheet_records}


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

    schedule = ski_root / "elo/python/nordic-combined/polars/excel365/races.csv"
    model = blog_root / "content/post/nordic-combined/drafts/race-picks-simulation.R"
    converter = blog_root / "static/python/excel_to_hugo_multiple_sheets.py"
    for required in (schedule, model, converter):
        if not required.is_file():
            raise PredictionShadowError(f"required runtime file is missing: {required}")

    run_root = output_root / "runs" / run_id
    if run_root.exists():
        raise PredictionShadowError(f"shadow run already exists: {run_root}")
    reports_root = run_root / "reports"
    workbooks_root = run_root / "workbooks"
    json_root = run_root / "json"
    logs_root = run_root / "logs"
    report_path = reports_root / "shadow-report.json"
    plan_path = reports_root / "plan.json"
    run_root.mkdir(parents=True)

    report: dict[str, object] = {
        "schema_version": SCHEMA_VERSION,
        "pipeline_id": "nordic_combined_prediction_shadow",
        "run_id": run_id,
        "logical_date": logical_date,
        "status": "failed",
        "code": {"blog_commit": blog_commit, "ski_commit": ski_commit},
        "parameters": {"n_simulations": n_simulations, "random_seed": random_seed},
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
            report["reason"] = "no Nordic Combined races are scheduled for the logical date"
            write_json_atomic(report_path, report)
            return report_path
        if plan["status"] != "ready_for_shadow_implementation":
            raise PredictionShadowError(f"prediction plan is blocked: {plan['blockers']}")

        individual_root, team_root, inputs = materialize_inputs(
            run_root=run_root,
            ski_root=ski_root,
            required_inputs=plan["required_ski_inputs"],
        )
        report["inputs"] = inputs
        expected_by_category: dict[str, list[set[str]]] = {}
        startlist_rows: dict[str, list[int]] = {}
        for category, count_value in plan["categories"].items():
            count = int(count_value)
            if not count:
                continue
            relative = _startlist_relative(category)
            root = team_root if "/relay/excel365/" in relative else individual_root
            identities = read_startlist_contract(
                root / Path(relative).name,
                category=category,
                count=count,
                logical_date=target_date,
            )
            expected_by_category[category] = identities
            startlist_rows[category] = [len(values) for values in identities]
        report["startlist_source"] = "accepted_repository"
        report["startlist_rows"] = startlist_rows

        environment = {
            **os.environ,
            "NORDIC_INPUT_ROOT": str(individual_root),
            "NORDIC_TEAM_INPUT_ROOT": str(team_root),
            "NORDIC_OUTPUT_ROOT": str(workbooks_root),
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
                [rscript, str(model)], cwd=blog_root, env=environment, text=True,
                stdout=handle, stderr=subprocess.STDOUT, check=False,
            )
        if result.returncode != 0:
            raise PredictionShadowError(f"R simulation failed with exit code {result.returncode}")

        dated_root = workbooks_root / str(plan["date_key"])
        expected_workbooks = {
            str(CATEGORY_CONTRACTS[category]["workbook"]): (category, int(count))
            for category, count in plan["categories"].items() if count
        }
        observed = {path.name for path in dated_root.glob("*.xlsx")}
        if observed != set(expected_workbooks):
            raise PredictionShadowError(
                f"workbook set mismatch: observed={sorted(observed)} expected={sorted(expected_workbooks)}"
            )
        workbook_records = []
        for filename, (category, count) in sorted(expected_workbooks.items()):
            workbook = dated_root / filename
            workbook_records.append(
                validate_workbook(
                    workbook,
                    category=category,
                    count=count,
                    expected_identities=expected_by_category[category],
                )
            )
            conversion = command_runner(
                [sys.executable, str(converter), str(workbook), str(json_root)],
                cwd=blog_root,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                check=False,
            )
            if conversion.returncode != 0:
                raise PredictionShadowError(
                    f"JSON conversion failed for {filename}: {conversion.stdout or ''}"
                )
        expected_json = set(plan["conversion"]["json_outputs"])
        observed_json = {path.name for path in json_root.glob("*.json")}
        if observed_json != expected_json:
            raise PredictionShadowError(
                f"JSON set mismatch: observed={sorted(observed_json)} expected={sorted(expected_json)}"
            )
        report["outputs"] = {
            "workbooks": workbook_records,
            "json": [
                {"file": path.name, "bytes": path.stat().st_size, "sha256": sha256(path)}
                for path in sorted(json_root.glob("*.json"))
            ],
        }
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
            rscript=args.rscript,
        )
    except Exception as error:  # noqa: BLE001 - CLI must expose retained failure
        print(f"Nordic Combined prediction shadow failed: {error}", file=sys.stderr)
        return 1
    print(report)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
