"""Validate a publication-disabled cross-country weekend shadow result."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import tempfile
from datetime import date
from pathlib import Path
from typing import Any

SHADOW_SCHEMA = "nordicnumbers.cross-country-weekend-shadow/v1"
VALIDATION_SCHEMA = "nordicnumbers.cross-country-weekend-validation/v1"
EXPECTED_HEADERS = (
    "Skier",
    "Nation",
    "Sex",
    "Predicted Weekend Points",
    "Races Count",
    "Avg Win Probability",
    "Avg Podium Probability",
)
SHEET_SEX = {"Men Top 30": "M", "Ladies Top 30": "L"}


class WeekendShadowError(RuntimeError):
    """The weekend simulation did not produce a trustworthy shadow result."""


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _atomic_json(path: Path, document: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", dir=path.parent, delete=False
    ) as handle:
        json.dump(document, handle, indent=2, sort_keys=True)
        handle.write("\n")
        temporary = Path(handle.name)
    os.replace(temporary, path)


def validate_weekend_shadow(
    *,
    runtime_dir: Path,
    prediction_root: Path,
    logical_date: date,
    n_simulations: int,
    random_seed: int,
    report_path: Path,
) -> dict[str, Any]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise WeekendShadowError(
            "openpyxl is required to validate the prediction workbook"
        ) from exc

    if n_simulations < 1:
        raise WeekendShadowError("n_simulations must be positive")
    date_key = logical_date.strftime("%Y%m%d")
    output_dir = prediction_root.expanduser().resolve() / date_key
    manifest_path = output_dir / "shadow_manifest.json"
    workbook_path = output_dir / "weekend_top_30_predicted_points.xlsx"
    runtime_report_path = runtime_dir.expanduser().resolve() / "runtime_report.json"

    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        runtime_report = json.loads(runtime_report_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise WeekendShadowError(f"cannot read shadow evidence: {exc}") from exc

    expected_manifest = {
        "schema_version": SHADOW_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date.isoformat(),
        "n_simulations": n_simulations,
        "random_seed": random_seed,
        "publication": {"status": "disabled"},
    }
    if manifest != expected_manifest:
        raise WeekendShadowError("shadow manifest does not match the invocation")
    if runtime_report.get("logical_date") != logical_date.isoformat():
        raise WeekendShadowError("runtime report logical date does not match")
    if runtime_report.get("publication") != {"status": "disabled"}:
        raise WeekendShadowError("runtime publication must remain disabled")

    expected_sheets = {
        SHEET_SEX[f"{gender} Top 30"]
        for gender in ("Men", "Ladies")
        if any(
            key.startswith(f"{SHEET_SEX[f'{gender} Top 30']}/")
            for key in runtime_report.get("event_modes", {})
        )
    }
    if not expected_sheets:
        raise WeekendShadowError("runtime report contains no individual events")

    try:
        workbook = load_workbook(workbook_path, read_only=True, data_only=True)
    except (OSError, ValueError) as exc:
        raise WeekendShadowError(f"cannot read prediction workbook: {exc}") from exc
    observed_sheets = set(workbook.sheetnames)
    required_sheets = {
        name for name, sex in SHEET_SEX.items() if sex in expected_sheets
    }
    if observed_sheets != required_sheets:
        raise WeekendShadowError(
            f"unexpected workbook sheets: {sorted(observed_sheets)}"
        )

    sheet_results: dict[str, dict[str, Any]] = {}
    for sheet_name in sorted(required_sheets):
        sheet = workbook[sheet_name]
        rows = sheet.iter_rows(values_only=True)
        headers = tuple(next(rows, ()))
        if headers != EXPECTED_HEADERS:
            raise WeekendShadowError(f"unexpected headers in {sheet_name}: {headers}")
        data = list(rows)
        if not 1 <= len(data) <= 30:
            raise WeekendShadowError(f"invalid row count in {sheet_name}: {len(data)}")
        expected_sex = SHEET_SEX[sheet_name]
        athletes: set[str] = set()
        for row in data:
            skier, nation, sex, points, races, win, podium = row
            if not skier or not nation or sex != expected_sex:
                raise WeekendShadowError(f"invalid identity in {sheet_name}")
            if skier in athletes:
                raise WeekendShadowError(f"duplicate athlete in {sheet_name}: {skier}")
            athletes.add(str(skier))
            if not isinstance(points, (int, float)) or not isinstance(
                races, (int, float)
            ):
                raise WeekendShadowError(f"invalid prediction values in {sheet_name}")
            if races < 1 or any(
                not isinstance(value, (int, float)) or not 0 <= value <= 100
                for value in (win, podium)
            ):
                raise WeekendShadowError(f"invalid probability values in {sheet_name}")
        sheet_results[sheet_name] = {"rows": len(data), "sex": expected_sex}
    workbook.close()

    report = {
        "schema_version": VALIDATION_SCHEMA,
        "status": "passed",
        "logical_date": logical_date.isoformat(),
        "publication": {"status": "disabled"},
        "n_simulations": n_simulations,
        "random_seed": random_seed,
        "sheets": sheet_results,
        "outputs": {
            "shadow_manifest.json": {"sha256": _sha256(manifest_path)},
            "weekend_top_30_predicted_points.xlsx": {"sha256": _sha256(workbook_path)},
        },
    }
    _atomic_json(report_path.expanduser().resolve(), report)
    return report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime-dir", required=True, type=Path)
    parser.add_argument("--prediction-root", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--n-simulations", required=True, type=int)
    parser.add_argument("--random-seed", required=True, type=int)
    parser.add_argument("--report", required=True, type=Path)
    args = parser.parse_args()
    report = validate_weekend_shadow(
        runtime_dir=args.runtime_dir,
        prediction_root=args.prediction_root,
        logical_date=args.logical_date,
        n_simulations=args.n_simulations,
        random_seed=args.random_seed,
        report_path=args.report,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
