"""Validate a deterministic, publication-disabled Final Climb shadow result."""

from __future__ import annotations

import argparse
import json
from datetime import date
from pathlib import Path
from typing import Any

from pipelines.cross_country_tour_shadow import (
    POINT_HEADERS,
    PROBABILITY_HEADERS,
    _athlete_id,
    _atomic_json,
    _sha256,
    _workbook_rows,
)

SHADOW_SCHEMA = "nordicnumbers.cross-country-final-climb-shadow/v1"
VALIDATION_SCHEMA = "nordicnumbers.cross-country-final-climb-validation/v1"
SEX_FILES = {
    "M": ("men.xlsx", "men_position_probabilities.xlsx", "Men Race 1"),
    "L": ("ladies.xlsx", "ladies_position_probabilities.xlsx", "Ladies Race 1"),
}


class FinalClimbShadowError(RuntimeError):
    """The Final Climb simulation did not produce a trustworthy shadow result."""


def _startlist_ids(runtime_dir: Path, sex: str) -> set[str]:
    import csv

    name = (
        "startlist_final_climb_men.csv"
        if sex == "M"
        else "startlist_final_climb_ladies.csv"
    )
    try:
        with (runtime_dir / name).open(newline="", encoding="utf-8-sig") as handle:
            rows = list(csv.DictReader(handle))
    except OSError as exc:
        raise FinalClimbShadowError(
            f"cannot read Final Climb runtime startlist {name}: {exc}"
        ) from exc
    if not rows or tuple(rows[0]) != ("ID", "Skier", "Nation", "Price"):
        raise FinalClimbShadowError(f"invalid runtime startlist: {name}")
    identities = {_athlete_id(row["ID"]) for row in rows}
    if "" in identities or len(identities) != len(rows):
        raise FinalClimbShadowError(f"invalid or duplicate runtime identity in {name}")
    return identities


def validate_final_climb_shadow(
    *,
    runtime_dir: Path,
    prediction_root: Path,
    logical_date: date,
    n_simulations: int,
    random_seed: int,
    report_path: Path,
) -> dict[str, Any]:
    if n_simulations < 1:
        raise FinalClimbShadowError("n_simulations must be positive")
    output_dir = prediction_root.expanduser().resolve() / logical_date.strftime(
        "%Y%m%d"
    )
    manifest_path = output_dir / "shadow_manifest.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        runtime_report = json.loads(
            (runtime_dir / "report.json").read_text(encoding="utf-8")
        )
    except (OSError, json.JSONDecodeError) as exc:
        raise FinalClimbShadowError(
            f"cannot read Final Climb shadow evidence: {exc}"
        ) from exc

    expected_manifest = {
        "schema_version": SHADOW_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date.isoformat(),
        "n_simulations": n_simulations,
        "random_seed": random_seed,
        "publication": {"status": "disabled"},
    }
    if manifest != expected_manifest:
        raise FinalClimbShadowError(
            "Final Climb shadow manifest does not match invocation"
        )
    if runtime_report.get("status") != "candidate":
        raise FinalClimbShadowError("Final Climb runtime is not a candidate")
    if runtime_report.get("publication") != {"status": "disabled"}:
        raise FinalClimbShadowError(
            "Final Climb runtime publication must remain disabled"
        )
    participant_source_mode = runtime_report.get("participant_source_mode")
    if participant_source_mode not in {
        "official_startlist",
        "historical_results_replay",
    }:
        raise FinalClimbShadowError(
            "Final Climb runtime participant source mode is invalid"
        )
    if (
        runtime_report.get("plan", {}).get("final_climb_date")
        != logical_date.isoformat()
    ):
        raise FinalClimbShadowError(
            "Final Climb runtime target date does not match invocation"
        )

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
            raise FinalClimbShadowError(f"unexpected headers in {points_name}")
        if not probability_rows or tuple(probability_rows[0]) != PROBABILITY_HEADERS:
            raise FinalClimbShadowError(f"unexpected headers in {probabilities_name}")

        point_ids: set[str] = set()
        for skier, athlete_id, nation, points in points_rows[1:]:
            athlete_id = _athlete_id(athlete_id)
            if not skier or not nation or not athlete_id or athlete_id in point_ids:
                raise FinalClimbShadowError(f"invalid identity in {points_name}")
            if not isinstance(points, (int, float)) or not 0 <= points <= 50:
                raise FinalClimbShadowError(f"invalid points in {points_name}")
            point_ids.add(athlete_id)

        probability_ids: set[str] = set()
        for row in probability_rows[1:]:
            skier, athlete_id, nation, *probabilities = row
            athlete_id = _athlete_id(athlete_id)
            if (
                not skier
                or not nation
                or not athlete_id
                or athlete_id in probability_ids
            ):
                raise FinalClimbShadowError(f"invalid identity in {probabilities_name}")
            if any(
                not isinstance(value, (int, float)) or not 0 <= value <= 100
                for value in probabilities
            ) or list(probabilities) != sorted(probabilities):
                raise FinalClimbShadowError(
                    f"invalid probabilities in {probabilities_name}"
                )
            probability_ids.add(athlete_id)

        if expected_ids != point_ids or expected_ids != probability_ids:
            raise FinalClimbShadowError(
                f"Final Climb output identities do not match {sex} startlist"
            )
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
        "participant_source_mode": participant_source_mode,
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
    report = validate_final_climb_shadow(
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
