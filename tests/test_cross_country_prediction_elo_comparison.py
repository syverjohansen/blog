import csv
import tempfile
import unittest
from pathlib import Path

from openpyxl import Workbook

from pipelines.cross_country_prediction_elo_comparison import (
    METADATA,
    METRICS,
    SHEETS,
    PredictionEloComparisonError,
    compare_prediction_elo_shadows,
)

FIELDS = (
    "Canonical Athlete ID",
    "FIS Code",
    "Skier",
    "Nation",
    "Sex",
    *METRICS,
    "Races Count",
    *METADATA,
)


def row(athlete_id: str, points: float, *, sex: str = "M"):
    return {
        "Canonical Athlete ID": athlete_id,
        "FIS Code": "3999999" if athlete_id.startswith("fis:") else "3000001",
        "Skier": (
            "Prediction Athlete" if athlete_id.startswith("fis:") else "Known Athlete"
        ),
        "Nation": "NOR",
        "Sex": sex,
        "Total Points": points,
        "Total Safe": points - 5,
        "Total Upside": points + 5,
        "Races Count": 1,
        "Logical Date": "2026-11-27",
        "Random Seed": 42,
        "Simulation Count": 250,
    }


def write_workbook(path: Path, men, ladies=None) -> None:
    workbook = Workbook()
    workbook.remove(workbook.active)
    groups = [(SHEETS[0], men)]
    if ladies is not None:
        groups.append((SHEETS[1], ladies))
    for name, rows in groups:
        sheet = workbook.create_sheet(name)
        sheet.append(FIELDS)
        for item in rows:
            sheet.append([item[field] for field in FIELDS])
    workbook.save(path)


class PredictionEloComparisonTests(unittest.TestCase):
    def test_reports_changes_only_for_prediction_identity(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline = root / "inputs/baseline.xlsx"
            candidate = root / "inputs/candidate.xlsx"
            baseline.parent.mkdir()
            write_workbook(
                baseline,
                [row("1", 70), row("fis:999", 25)],
                [row("2", 60, sex="L")],
            )
            write_workbook(
                candidate,
                [row("1", 70), row("fis:999", 32)],
                [row("2", 60, sex="L")],
            )
            report = compare_prediction_elo_shadows(
                baseline=baseline,
                candidate=candidate,
                output_dir=root / "comparison",
            )
            self.assertEqual(
                report["status"],
                "shadow_differences_observed",
            )
            self.assertEqual(report["changed_prediction_identities"], ["fis:999"])
            self.assertEqual(report["maximum_absolute_delta"]["Total Points"], 7)
            self.assertEqual(report["publication"], {"status": "disabled"})
            with (root / "comparison/prediction_elo_differences.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = list(csv.DictReader(handle))
            changed = [item for item in rows if item["changed"] == "true"]
            self.assertEqual([item["athlete_id"] for item in changed], ["fis:999"])

    def test_reports_downstream_change_to_accepted_history_athlete(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline = root / "inputs/baseline.xlsx"
            candidate = root / "inputs/candidate.xlsx"
            baseline.parent.mkdir()
            write_workbook(baseline, [row("1", 70)], [row("2", 60, sex="L")])
            write_workbook(candidate, [row("1", 71)], [row("2", 60, sex="L")])
            report = compare_prediction_elo_shadows(
                baseline=baseline,
                candidate=candidate,
                output_dir=root / "comparison",
            )
            self.assertEqual(report["changed_prediction_identities"], [])
            self.assertEqual(report["changed_accepted_history_identities"], ["1"])

    def test_rejects_different_seed(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline = root / "inputs/baseline.xlsx"
            candidate = root / "inputs/candidate.xlsx"
            baseline.parent.mkdir()
            known = row("1", 70)
            changed = dict(known)
            changed["Random Seed"] = 99
            write_workbook(baseline, [known], [row("2", 60, sex="L")])
            write_workbook(candidate, [changed], [row("2", 60, sex="L")])
            with self.assertRaisesRegex(
                PredictionEloComparisonError, "immutable field changed"
            ):
                compare_prediction_elo_shadows(
                    baseline=baseline,
                    candidate=candidate,
                    output_dir=root / "comparison",
                )

    def test_allows_single_sex_weekend(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            baseline = root / "inputs/baseline.xlsx"
            candidate = root / "inputs/candidate.xlsx"
            baseline.parent.mkdir()
            write_workbook(baseline, [row("1", 70), row("fis:999", 25)])
            write_workbook(candidate, [row("1", 70), row("fis:999", 30)])
            report = compare_prediction_elo_shadows(
                baseline=baseline,
                candidate=candidate,
                output_dir=root / "comparison",
            )
            self.assertEqual(report["changed_prediction_identities"], ["fis:999"])


if __name__ == "__main__":
    unittest.main()
