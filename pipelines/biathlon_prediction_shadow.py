"""Run exact-date Biathlon predictions in an isolated publication-disabled workspace."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
from datetime import date, datetime
from pathlib import Path
from typing import Any, Callable

from openpyxl import load_workbook


SCHEMA_VERSION = "nordicnumbers.biathlon-prediction-shadow/v1"
NORMALIZATION_SCHEMA = "nordicnumbers.biathlon-startlist-normalization/v1"
SAFE_RUN_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")
INDIVIDUAL_COLUMNS = (
    "Skier", "ID", "Nation", "Sex", "Participation", "Win", "Podium",
    "Top-5", "Top-10", "Top-30",
)
TEAM_COLUMNS = (
    "Nation", "Sex", "RaceType", "Win", "Podium", "Top-5", "Top-10", "Top-30",
)
PROBABILITY_COLUMNS = ("Win", "Podium", "Top-5", "Top-10", "Top-30")
CATEGORIES = {
    "men": ("startlist_races_men.csv", "men_position_probabilities.xlsx", False),
    "ladies": ("startlist_races_ladies.csv", "ladies_position_probabilities.xlsx", False),
    "men_relay": ("startlist_relay_races_teams_men.csv", "men_relay_position_probabilities.xlsx", True),
    "ladies_relay": ("startlist_relay_races_teams_ladies.csv", "ladies_relay_position_probabilities.xlsx", True),
    "mixed_relay": ("startlist_mixed_relay_races_teams.csv", "mixed_relay_position_probabilities.xlsx", True),
    "single_mixed_relay": ("startlist_single_mixed_relay_races_teams.csv", "single_mixed_relay_position_probabilities.xlsx", True),
}


class BiathlonShadowError(RuntimeError):
    """The shadow cannot continue without weakening its evidence."""


def _json_atomic(path: Path, document: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _source_date(value: str) -> date:
    for pattern in ("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), pattern).date()
        except ValueError:
            pass
    raise BiathlonShadowError(f"unsupported schedule date: {value!r}")


def _category(row: dict[str, str]) -> str:
    race_type, sex = row.get("RaceType", ""), row.get("Sex", "")
    if race_type == "Relay":
        return "men_relay" if sex == "M" else "ladies_relay"
    if race_type == "Mixed Relay":
        return "mixed_relay"
    if race_type == "Single Mixed Relay":
        return "single_mixed_relay"
    if sex == "M":
        return "men"
    if sex == "L":
        return "ladies"
    raise BiathlonShadowError(f"unsupported Biathlon schedule row: {row}")


def build_plan(schedule: Path, logical_date: date) -> dict[str, Any]:
    with schedule.open(newline="", encoding="utf-8-sig") as handle:
        selected = [row for row in csv.DictReader(handle) if _source_date(row["Date"]) == logical_date]
    counts = {category: 0 for category in CATEGORIES}
    for row in selected:
        counts[_category(row)] += 1
    return {
        "schema_version": "nordicnumbers.biathlon-prediction-plan/v1",
        "status": "ready" if selected else "no_races",
        "logical_date": logical_date.isoformat(),
        "date_key": logical_date.strftime("%Y%m%d"),
        "categories": counts,
        "races": selected,
    }


def _candidate_identities(path: Path, category: str, race_count: int) -> list[set[str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        fields = set(reader.fieldnames or ())
        rows = list(reader)
    is_team = CATEGORIES[category][2]
    required = {"Nation"} if is_team else {"ID", "Skier", "Nation"}
    if missing := sorted(required - fields):
        raise BiathlonShadowError(f"{category} candidate lacks fields {missing}")
    if not rows:
        raise BiathlonShadowError(f"{category} candidate is empty")
    if is_team:
        identities = {row["Nation"].strip() for row in rows}
        if "" in identities or len(identities) != len(rows):
            raise BiathlonShadowError(f"{category} has blank or duplicate teams")
        return [identities]
    result: list[set[str]] = []
    for index in range(1, race_count + 1):
        column = f"Race{index}_Prob"
        active = rows if column not in fields else [
            row for row in rows if float(row.get(column, "0") or 0) > 0
        ]
        identities = {str(row["ID"]).strip() for row in active}
        if "" in identities or len(identities) != len(active) or not identities:
            raise BiathlonShadowError(f"{category} race {index} has invalid identities")
        result.append(identities)
    return result


def validate_normalization(
    report_path: Path, candidate_root: Path, plan: dict[str, Any]
) -> tuple[list[dict[str, Any]], dict[str, list[set[str]]]]:
    report = json.loads(report_path.read_text(encoding="utf-8"))
    expected_categories = {category for category, count in plan["categories"].items() if count}
    if (
        report.get("schema_version") != NORMALIZATION_SCHEMA
        or report.get("status") != "ready"
        or report.get("prediction_eligible") is not True
        or report.get("logical_date") != plan["logical_date"]
        or report.get("accepted_startlists_modified") is not False
        or set(report.get("outputs", {})) != expected_categories
    ):
        raise BiathlonShadowError("startlist normalization is not publication eligible")
    records: list[dict[str, Any]] = []
    identities: dict[str, list[set[str]]] = {}
    for category in sorted(expected_categories):
        metadata = report["outputs"][category]
        target = (candidate_root / CATEGORIES[category][0]).resolve()
        reported = (report_path.parent / metadata["file"]).resolve()
        if target != reported or not target.is_file() or _sha256(target) != metadata["sha256"]:
            raise BiathlonShadowError(f"normalized candidate evidence mismatch for {category}")
        with target.open(newline="", encoding="utf-8-sig") as handle:
            rows = sum(1 for _ in csv.reader(handle)) - 1
        if rows != metadata["rows"]:
            raise BiathlonShadowError(f"normalized row count mismatch for {category}")
        identities[category] = _candidate_identities(target, category, int(plan["categories"][category]))
        records.append({"category": category, "file": str(target), "rows": rows, "sha256": metadata["sha256"]})
    return records, identities


def _copy(source: Path, target: Path) -> dict[str, Any]:
    if not source.is_file():
        raise BiathlonShadowError(f"required input is missing: {source}")
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, target)
    return {"file": str(target), "sha256": _sha256(target)}


def _materialize(
    *, ski_root: Path, run_root: Path, candidate_root: Path,
    active: set[str],
) -> tuple[Path, Path, list[dict[str, Any]]]:
    accepted = ski_root / "elo/python/biathlon/polars/excel365"
    accepted_relay = ski_root / "elo/python/biathlon/polars/relay/excel365"
    individual = run_root / "inputs/individual"
    relay = run_root / "inputs/relay"
    records = [
        _copy(accepted / "races.csv", individual / "races.csv"),
        _copy(accepted / "men_chrono.csv", individual / "men_chrono.csv"),
        _copy(accepted / "ladies_chrono.csv", individual / "ladies_chrono.csv"),
    ]
    for filename in (
        "men_relay_chrono.csv", "ladies_relay_chrono.csv",
        "mixed_relay_chrono.csv", "single_mixed_relay_chrono.csv",
    ):
        records.append(_copy(accepted_relay / filename, relay / filename))
    for category in active:
        filename = CATEGORIES[category][0]
        target_root = relay if CATEGORIES[category][2] else individual
        records.append(_copy(candidate_root / filename, target_root / filename))
    return individual, relay, records


def _validate_workbook(
    path: Path, *, category: str, expected: list[set[str]]
) -> dict[str, Any]:
    if not path.is_file():
        raise BiathlonShadowError(f"missing prediction workbook: {path.name}")
    workbook = load_workbook(path, read_only=True, data_only=True)
    sheets: dict[str, int] = {}
    try:
        if len(workbook.sheetnames) != len(expected):
            raise BiathlonShadowError(f"sheet count mismatch for {category}")
        for sheet_name, expected_ids in zip(workbook.sheetnames, expected, strict=True):
            sheet = workbook[sheet_name]
            if hasattr(sheet, "reset_dimensions"):
                sheet.reset_dimensions()
            rows = sheet.iter_rows(values_only=True)
            headers = tuple(next(rows, ()))
            required = TEAM_COLUMNS if CATEGORIES[category][2] else INDIVIDUAL_COLUMNS
            if headers != required:
                raise BiathlonShadowError(f"workbook schema mismatch for {category}/{sheet_name}")
            values = [row for row in rows if any(value is not None for value in row)]
            id_index = headers.index("Nation" if CATEGORIES[category][2] else "ID")
            observed = {str(row[id_index]).removesuffix(".0") for row in values}
            if observed != expected_ids or len(observed) != len(values):
                raise BiathlonShadowError(f"workbook roster mismatch for {category}/{sheet_name}")
            win_total = 0.0
            probability_indexes = [headers.index(column) for column in PROBABILITY_COLUMNS]
            for row in values:
                probabilities = [float(row[index]) for index in probability_indexes]
                if probabilities != sorted(probabilities) or any(value < 0 or value > 100 for value in probabilities):
                    raise BiathlonShadowError(f"invalid probabilities for {category}/{sheet_name}")
                win_total += probabilities[0]
            if not 95 <= win_total <= 105:
                raise BiathlonShadowError(f"winner probability sum failed for {category}/{sheet_name}: {win_total}")
            sheets[sheet_name] = len(values)
    finally:
        workbook.close()
    return {"category": category, "file": path.name, "sha256": _sha256(path), "sheets": sheets}


def run_shadow(
    *, blog_root: Path, ski_root: Path, output_root: Path, logical_date: str,
    run_id: str, n_simulations: int, random_seed: int, blog_commit: str,
    ski_commit: str, startlist_root: Path, startlist_report: Path,
    rscript: str = "Rscript",
    command_runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
) -> Path:
    if not SAFE_RUN_ID.fullmatch(run_id) or n_simulations < 1:
        raise BiathlonShadowError("invalid run parameters")
    for revision in (blog_commit, ski_commit):
        if not re.fullmatch(r"[0-9a-f]{40}", revision):
            raise BiathlonShadowError("code revisions must be full commit SHAs")
    day = date.fromisoformat(logical_date)
    blog_root, ski_root, output_root = (
        value.expanduser().resolve() for value in (blog_root, ski_root, output_root)
    )
    if any(output_root == root or root in output_root.parents for root in (blog_root, ski_root)):
        raise BiathlonShadowError("output root must be outside both repositories")
    schedule = ski_root / "elo/python/biathlon/polars/excel365/races.csv"
    model = blog_root / "content/post/biathlon/drafts/race-picks-simulation.R"
    run_root = output_root / "runs" / run_id
    if run_root.exists():
        raise BiathlonShadowError(f"shadow run already exists: {run_root}")
    report_path = run_root / "reports/shadow-report.json"
    report: dict[str, Any] = {
        "schema_version": SCHEMA_VERSION, "pipeline_id": "biathlon_prediction_shadow",
        "run_id": run_id, "logical_date": logical_date, "status": "failed",
        "code": {"blog_commit": blog_commit, "ski_commit": ski_commit},
        "parameters": {"n_simulations": n_simulations, "random_seed": random_seed},
        "publication": {"status": "disabled", "reason": "shadow runs cannot publish"},
    }
    try:
        plan = build_plan(schedule, day)
        _json_atomic(run_root / "reports/plan.json", plan)
        report["plan"] = "reports/plan.json"
        if plan["status"] == "no_races":
            report.update(status="skipped", reason="no Biathlon races are scheduled")
            _json_atomic(report_path, report)
            return report_path
        normalized, identities = validate_normalization(startlist_report, startlist_root, plan)
        active = {category for category, count in plan["categories"].items() if count}
        individual, relay, inputs = _materialize(
            ski_root=ski_root, run_root=run_root, candidate_root=startlist_root, active=active
        )
        report["inputs"] = inputs
        report["startlist_source"] = "normalized_official_ibu"
        report["startlist_normalization_report"] = {
            "file": str(startlist_report), "sha256": _sha256(startlist_report),
        }
        report["normalized_startlists"] = normalized
        workbooks = run_root / "workbooks"
        logs = run_root / "logs"
        logs.mkdir(parents=True, exist_ok=True)
        environment = {
            **os.environ, "NORDIC_INPUT_ROOT": str(individual),
            "NORDIC_RELAY_INPUT_ROOT": str(relay), "NORDIC_OUTPUT_ROOT": str(workbooks),
            "NORDIC_BLOG_ROOT": str(blog_root), "NORDIC_LOG_ROOT": str(logs),
            "NORDIC_LOGICAL_DATE": logical_date, "NORDIC_RUN_ID": run_id,
            "NORDIC_RANDOM_SEED": str(random_seed), "NORDIC_N_SIMULATIONS": str(n_simulations),
            "NORDIC_LOAD_ENV": "false", "TEST_MODE": "false",
        }
        with (logs / "simulation-process.log").open("w", encoding="utf-8") as handle:
            result = command_runner(
                [rscript, str(model)], cwd=blog_root, env=environment,
                stdout=handle, stderr=subprocess.STDOUT, text=True, check=False,
            )
        if result.returncode != 0:
            raise BiathlonShadowError(f"R simulation failed with exit code {result.returncode}")
        dated = workbooks / plan["date_key"]
        expected_files = {CATEGORIES[category][1]: category for category in active}
        observed = {path.name for path in dated.glob("*.xlsx")}
        if observed != set(expected_files):
            raise BiathlonShadowError(f"workbook set mismatch: {sorted(observed)} != {sorted(expected_files)}")
        report["outputs"] = [
            _validate_workbook(dated / filename, category=category, expected=identities[category])
            for filename, category in sorted(expected_files.items())
        ]
        report["status"] = "passed"
    except Exception as error:
        report["error"] = str(error)
        _json_atomic(report_path, report)
        raise
    _json_atomic(report_path, report)
    return report_path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--blog-root", type=Path, required=True)
    parser.add_argument("--ski-root", type=Path, required=True)
    parser.add_argument("--output-root", type=Path, required=True)
    parser.add_argument("--logical-date", required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--n-simulations", type=int, required=True)
    parser.add_argument("--random-seed", type=int, required=True)
    parser.add_argument("--blog-commit", required=True)
    parser.add_argument("--ski-commit", required=True)
    parser.add_argument("--startlist-root", type=Path, required=True)
    parser.add_argument("--startlist-report", type=Path, required=True)
    args = parser.parse_args()
    print(run_shadow(**vars(args)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
