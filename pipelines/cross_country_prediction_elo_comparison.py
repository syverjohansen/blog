"""Compare baseline and private-Elo cross-country shadow predictions.

Both workbooks must come from the same startlist, logical date, simulation
count, and random seed. Changes to ``fis:`` prediction identities are reported
separately from the downstream ranking/points spillover they can cause for
accepted-history athletes. The report is evidence for review and is never a
publication approval.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import os
import tempfile
from pathlib import Path
from typing import Any

from openpyxl import load_workbook

REPORT_SCHEMA = "nordicnumbers.cross-country-prediction-elo-comparison/v1"
SHEETS = ("Men Full Pool", "Ladies Full Pool")
KEY = "Canonical Athlete ID"
METRICS = ("Total Points", "Total Safe", "Total Upside")
METADATA = ("Logical Date", "Random Seed", "Simulation Count")


class PredictionEloComparisonError(RuntimeError):
    """The two shadow prediction runs cannot be compared safely."""


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _atomic_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", newline="", dir=path.parent, delete=False
    ) as handle:
        handle.write(text)
        handle.flush()
        os.fsync(handle.fileno())
        temporary = Path(handle.name)
    os.replace(temporary, path)


def _read_workbook(path: Path) -> dict[str, list[dict[str, Any]]]:
    try:
        workbook = load_workbook(path, read_only=True, data_only=True)
    except Exception as exc:
        raise PredictionEloComparisonError(
            f"cannot read workbook {path}: {exc}"
        ) from exc
    unexpected = set(workbook.sheetnames) - set(SHEETS)
    selected_sheets = tuple(name for name in SHEETS if name in workbook.sheetnames)
    if unexpected or not selected_sheets:
        raise PredictionEloComparisonError(
            f"unexpected workbook sheets: {workbook.sheetnames}"
        )
    result = {}
    required = {
        KEY,
        "FIS Code",
        "Skier",
        "Nation",
        "Sex",
        *METRICS,
        "Races Count",
        *METADATA,
    }
    for sheet_name in selected_sheets:
        sheet = workbook[sheet_name]
        values = sheet.iter_rows(values_only=True)
        try:
            fields = tuple(str(value) for value in next(values))
        except StopIteration as exc:
            raise PredictionEloComparisonError(f"empty sheet: {sheet_name}") from exc
        if set(fields) != required:
            raise PredictionEloComparisonError(
                f"unexpected fields in {sheet_name}: {fields}"
            )
        rows = [dict(zip(fields, row, strict=True)) for row in values]
        if not rows:
            raise PredictionEloComparisonError(f"empty prediction sheet: {sheet_name}")
        result[sheet_name] = rows
    workbook.close()
    return result


def _index(
    rows: list[dict[str, Any]], sheet: str
) -> dict[str, tuple[int, dict[str, Any]]]:
    indexed = {}
    for rank, row in enumerate(rows, start=1):
        athlete_id = str(row[KEY]).strip()
        if not athlete_id or athlete_id in indexed:
            raise PredictionEloComparisonError(
                f"missing or duplicate athlete identity in {sheet}: {athlete_id!r}"
            )
        indexed[athlete_id] = (rank, row)
    return indexed


def compare_prediction_elo_shadows(
    *, baseline: Path, candidate: Path, output_dir: Path
) -> dict[str, object]:
    baseline = baseline.expanduser().resolve()
    candidate = candidate.expanduser().resolve()
    output_dir = output_dir.expanduser().resolve()
    for source in (baseline, candidate):
        parent = source.parent
        if output_dir == parent or parent in output_dir.parents:
            raise PredictionEloComparisonError(
                "comparison output cannot overlap prediction inputs"
            )
    baseline_sheets = _read_workbook(baseline)
    candidate_sheets = _read_workbook(candidate)
    if tuple(baseline_sheets) != tuple(candidate_sheets):
        raise PredictionEloComparisonError("prediction workbook sheets changed")
    differences: list[dict[str, object]] = []
    changed_prediction_ids: set[str] = set()
    changed_accepted_ids: set[str] = set()
    unchanged = 0

    for sheet in baseline_sheets:
        before = _index(baseline_sheets[sheet], sheet)
        after = _index(candidate_sheets[sheet], sheet)
        if set(before) != set(after):
            raise PredictionEloComparisonError(f"athlete set changed in {sheet}")
        for athlete_id in sorted(before):
            baseline_rank, baseline_row = before[athlete_id]
            candidate_rank, candidate_row = after[athlete_id]
            for field in (
                "FIS Code",
                "Skier",
                "Nation",
                "Sex",
                "Races Count",
                *METADATA,
            ):
                if str(baseline_row[field]) != str(candidate_row[field]):
                    raise PredictionEloComparisonError(
                        f"{sheet}/{athlete_id}: immutable field changed: {field}"
                    )
            deltas = {
                metric: float(candidate_row[metric]) - float(baseline_row[metric])
                for metric in METRICS
            }
            changed = candidate_rank != baseline_rank or any(
                delta != 0.0 for delta in deltas.values()
            )
            if changed:
                if athlete_id.startswith("fis:"):
                    changed_prediction_ids.add(athlete_id)
                else:
                    changed_accepted_ids.add(athlete_id)
            else:
                unchanged += 1
            differences.append(
                {
                    "sheet": sheet,
                    "athlete_id": athlete_id,
                    "fis_code": baseline_row["FIS Code"],
                    "skier": baseline_row["Skier"],
                    "baseline_rank": baseline_rank,
                    "candidate_rank": candidate_rank,
                    "rank_delta": candidate_rank - baseline_rank,
                    **{
                        f"baseline_{metric.lower().replace(' ', '_')}": baseline_row[
                            metric
                        ]
                        for metric in METRICS
                    },
                    **{
                        f"candidate_{metric.lower().replace(' ', '_')}": candidate_row[
                            metric
                        ]
                        for metric in METRICS
                    },
                    **{
                        f"delta_{metric.lower().replace(' ', '_')}": deltas[metric]
                        for metric in METRICS
                    },
                    "changed": str(changed).lower(),
                }
            )

    fields = list(differences[0])
    output = io.StringIO(newline="")
    writer = csv.DictWriter(output, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(differences)
    detail_text = output.getvalue()
    _atomic_text(output_dir / "prediction_elo_differences.csv", detail_text)
    report: dict[str, object] = {
        "schema_version": REPORT_SCHEMA,
        "status": (
            "shadow_differences_observed"
            if changed_prediction_ids or changed_accepted_ids
            else "no_prediction_only_impact"
        ),
        "publication": {"status": "disabled"},
        "baseline_sha256": _sha256(baseline),
        "candidate_sha256": _sha256(candidate),
        "athlete_rows": len(differences),
        "changed_prediction_identities": sorted(changed_prediction_ids),
        "changed_prediction_athletes": len(changed_prediction_ids),
        "changed_accepted_history_identities": sorted(changed_accepted_ids),
        "changed_accepted_history_athletes": len(changed_accepted_ids),
        "unchanged_athlete_rows": unchanged,
        "maximum_absolute_delta": {
            metric: max(
                abs(float(row[f"delta_{metric.lower().replace(' ', '_')}"]))
                for row in differences
            )
            for metric in METRICS
        },
        "details": {
            "path": "prediction_elo_differences.csv",
            "sha256": hashlib.sha256(detail_text.encode()).hexdigest(),
        },
    }
    _atomic_text(
        output_dir / "report.json",
        json.dumps(report, indent=2, sort_keys=True) + "\n",
    )
    return report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", required=True, type=Path)
    parser.add_argument("--candidate", required=True, type=Path)
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    report = compare_prediction_elo_shadows(
        baseline=args.baseline,
        candidate=args.candidate,
        output_dir=args.output_dir,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
