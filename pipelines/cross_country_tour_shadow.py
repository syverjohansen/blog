"""Validate a deterministic, publication-disabled Tour de Ski shadow result."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import tempfile
from datetime import date
from pathlib import Path
from typing import Any

SHADOW_SCHEMA = "nordicnumbers.cross-country-tour-shadow/v1"
VALIDATION_SCHEMA = "nordicnumbers.cross-country-tour-validation/v1"
POINT_HEADERS = ("Skier", "ID", "Nation", "Points")
PROBABILITY_HEADERS = (
    "Skier",
    "ID",
    "Nation",
    "Win",
    "Podium",
    "Top-5",
    "Top-10",
    "Top-30",
)
SEX_FILES = {
    "M": ("men.xlsx", "men_position_probabilities.xlsx", "Men Tour de Ski"),
    "L": (
        "ladies.xlsx",
        "ladies_position_probabilities.xlsx",
        "Ladies Tour de Ski",
    ),
}


class TourShadowError(RuntimeError):
    """The Tour simulation did not produce a trustworthy shadow result."""


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
        temporary = Path(handle.name)
        json.dump(document, handle, indent=2, sort_keys=True)
        handle.write("\n")
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, path)


def _athlete_id(value: object) -> str:
    if isinstance(value, float) and value.is_integer():
        return str(int(value))
    return str(value or "").strip()


def _startlist_ids(runtime_dir: Path, sex: str) -> set[str]:
    name = "startlist_tds_men.csv" if sex == "M" else "startlist_tds_ladies.csv"
    try:
        with (runtime_dir / name).open(newline="", encoding="utf-8-sig") as handle:
            rows = list(csv.DictReader(handle))
    except OSError as exc:
        raise TourShadowError(f"cannot read runtime startlist {name}: {exc}") from exc
    if not rows or tuple(rows[0]) != ("ID", "Skier", "Nation", "Price"):
        raise TourShadowError(f"invalid runtime startlist: {name}")
    identities = {_athlete_id(row["ID"]) for row in rows}
    if "" in identities or len(identities) != len(rows):
        raise TourShadowError(f"invalid or duplicate runtime identity in {name}")
    return identities


def _workbook_rows(path: Path, expected_sheet: str | None) -> list[tuple[object, ...]]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise TourShadowError("openpyxl is required to validate Tour output") from exc
    try:
        workbook = load_workbook(path, read_only=True, data_only=True)
    except (OSError, ValueError) as exc:
        raise TourShadowError(f"cannot read Tour workbook {path}: {exc}") from exc
    if expected_sheet is not None and workbook.sheetnames != [expected_sheet]:
        workbook.close()
        raise TourShadowError(
            f"unexpected sheets in {path.name}: {workbook.sheetnames}"
        )
    sheet = workbook[expected_sheet] if expected_sheet else workbook.active
    rows = list(sheet.iter_rows(values_only=True))
    workbook.close()
    return rows


def validate_tour_shadow(
    *,
    runtime_dir: Path,
    prediction_root: Path,
    logical_date: date,
    n_simulations: int,
    random_seed: int,
    report_path: Path,
) -> dict[str, Any]:
    if n_simulations < 1:
        raise TourShadowError("n_simulations must be positive")
    output_dir = prediction_root.expanduser().resolve() / logical_date.strftime("%Y%m%d")
    manifest_path = output_dir / "shadow_manifest.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        runtime_report = json.loads(
            (runtime_dir / "report.json").read_text(encoding="utf-8")
        )
    except (OSError, json.JSONDecodeError) as exc:
        raise TourShadowError(f"cannot read Tour shadow evidence: {exc}") from exc

    expected_manifest = {
        "schema_version": SHADOW_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date.isoformat(),
        "n_simulations": n_simulations,
        "random_seed": random_seed,
        "publication": {"status": "disabled"},
    }
    if manifest != expected_manifest:
        raise TourShadowError("Tour shadow manifest does not match invocation")
    if runtime_report.get("status") != "candidate":
        raise TourShadowError("Tour runtime is not a candidate")
    if runtime_report.get("publication") != {"status": "disabled"}:
        raise TourShadowError("Tour runtime publication must remain disabled")
    if runtime_report.get("logical_date") != logical_date.isoformat():
        raise TourShadowError("Tour runtime logical date does not match invocation")

    results: dict[str, dict[str, Any]] = {}
    outputs: dict[str, dict[str, str]] = {
        "shadow_manifest.json": {"sha256": _sha256(manifest_path)}
    }
    for sex, (points_name, probabilities_name, probability_sheet) in SEX_FILES.items():
        expected_ids = _startlist_ids(runtime_dir, sex)
        points_path = output_dir / points_name
        probability_path = output_dir / probabilities_name
        points_rows = _workbook_rows(points_path, None)
        probability_rows = _workbook_rows(probability_path, probability_sheet)
        if not points_rows or tuple(points_rows[0]) != POINT_HEADERS:
            raise TourShadowError(f"unexpected headers in {points_name}")
        if not probability_rows or tuple(probability_rows[0]) != PROBABILITY_HEADERS:
            raise TourShadowError(f"unexpected headers in {probabilities_name}")

        point_ids: set[str] = set()
        for skier, athlete_id, nation, points in points_rows[1:]:
            athlete_id = _athlete_id(athlete_id)
            if not skier or not nation or not athlete_id or athlete_id in point_ids:
                raise TourShadowError(f"invalid identity in {points_name}")
            if not isinstance(points, (int, float)) or not 0 <= points <= 300:
                raise TourShadowError(f"invalid points in {points_name}")
            point_ids.add(athlete_id)

        probability_ids: set[str] = set()
        for row in probability_rows[1:]:
            skier, athlete_id, nation, *probabilities = row
            athlete_id = _athlete_id(athlete_id)
            if not skier or not nation or not athlete_id or athlete_id in probability_ids:
                raise TourShadowError(f"invalid identity in {probabilities_name}")
            if any(
                not isinstance(value, (int, float)) or not 0 <= value <= 100
                for value in probabilities
            ) or list(probabilities) != sorted(probabilities):
                raise TourShadowError(f"invalid probabilities in {probabilities_name}")
            probability_ids.add(athlete_id)

        if expected_ids != point_ids or expected_ids != probability_ids:
            raise TourShadowError(f"Tour output identities do not match {sex} startlist")
        results[sex] = {"rows": len(expected_ids)}
        outputs[points_name] = {"sha256": _sha256(points_path)}
        outputs[probabilities_name] = {"sha256": _sha256(probability_path)}

    report = {
        "schema_version": VALIDATION_SCHEMA,
        "status": "passed",
        "logical_date": logical_date.isoformat(),
        "n_simulations": n_simulations,
        "random_seed": random_seed,
        "publication": {"status": "disabled"},
        "sexes": results,
        "outputs": outputs,
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
    report = validate_tour_shadow(
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
