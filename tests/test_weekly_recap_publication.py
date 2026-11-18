import csv
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.weekly_recap_publication import (
    WeeklyRecapError,
    detect_recap_sports,
    prepare_cross_country_history,
    render_post,
    validate_sport_artifacts,
    write_post,
)


def _write_schedule(path: Path, dates: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=("Date", "City"))
        writer.writeheader()
        writer.writerows({"Date": value, "City": "Example"} for value in dates)


def _write_artifacts(root: Path, sport: str, *, empty: str | None = None) -> None:
    for gender in ("men", "ladies"):
        for artifact_type in (
            "elo_change",
            "standings_predictions",
            "magic_numbers",
        ):
            path = root / sport / f"{gender}_{artifact_type}.json"
            path.parent.mkdir(parents=True, exist_ok=True)
            rows = [] if empty == f"{gender}_{artifact_type}" else [["Athlete", 1]]
            path.write_text(
                json.dumps({"headers": ["Skier", "Value"], "rows": rows}),
                encoding="utf-8",
            )


class WeeklyRecapPublicationTests(unittest.TestCase):
    def test_detects_supported_sports_with_races_in_previous_week(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_schedule(
                root / "alpine/polars/excel365/races.csv",
                ["11/22/2026"],
            )
            _write_schedule(
                root / "ski/polars/excel365/races.csv",
                ["11/15/2026"],
            )
            sports = detect_recap_sports(root, date(2026, 11, 23))
            self.assertEqual([sport.content_name for sport in sports], ["alpine"])

    def test_non_monday_is_always_a_no_op(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_schedule(
                root / "alpine/polars/excel365/races.csv",
                ["11/17/2026"],
            )
            _write_schedule(
                root / "ski/polars/excel365/races.csv",
                ["11/17/2026"],
            )
            self.assertEqual(detect_recap_sports(root, date(2026, 11, 18)), ())

    def test_validates_six_complete_artifacts_per_sport(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_artifacts(root, "cross-country")
            self.assertEqual(len(validate_sport_artifacts(root, "cross-country")), 6)
            _write_artifacts(root, "alpine", empty="ladies_magic_numbers")
            with self.assertRaisesRegex(WeeklyRecapError, "has no rows"):
                validate_sport_artifacts(root, "alpine")

    def test_prepares_current_cross_country_elevation_histories(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            fields = ("Date", "City", "Skier", "ID")
            for gender in ("men", "ladies"):
                path = root / f"{gender}_chrono.csv"
                with path.open("w", newline="", encoding="utf-8") as handle:
                    writer = csv.DictWriter(handle, fieldnames=fields)
                    writer.writeheader()
                    writer.writerow(
                        {"Date": "2026-11-22", "City": "Ruka", "Skier": "A", "ID": 1}
                    )
            with (root / "elevation.csv").open(
                "w", newline="", encoding="utf-8"
            ) as handle:
                writer = csv.DictWriter(handle, fieldnames=("City", "Elevation"))
                writer.writeheader()
                writer.writerow({"City": "Ruka", "Elevation": 200})

            prepare_cross_country_history(root)

            with (root / "men_chrono_elevation.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual(rows[0]["Elevation"], "200")

    def test_renders_deterministic_combined_post(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_artifacts(root, "alpine")
            _write_artifacts(root, "cross-country")
            rendered = render_post(
                date(2026, 11, 23), ("alpine", "cross-country"), root
            )
            self.assertIn("11/16/2026 to 11/22/2026", rendered)
            self.assertIn("## Alpine", rendered)
            self.assertIn("## Cross Country", rendered)
            self.assertIn("cross-country/drafts/weekly-recap/20261123", rendered)
            output = root / "post.md"
            write_post(output, rendered)
            self.assertEqual(output.read_text(encoding="utf-8"), rendered)


if __name__ == "__main__":
    unittest.main()
