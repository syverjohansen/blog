from __future__ import annotations

import csv
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.test_pipeline import (
    ALPINE_RACE_HEADERS,
    PipelineCheckError,
    validate_current_alpine_inputs,
)


class AlpineCurrentInputTests(unittest.TestCase):
    logical_date = date(2026, 11, 28)

    def build_inputs(self, root: Path) -> Path:
        with (root / "races.csv").open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=ALPINE_RACE_HEADERS)
            writer.writeheader()
            for sex, race in (("M", "1"), ("L", "1")):
                writer.writerow(
                    {
                        "Date": "11/28/2026",
                        "Startlist": (
                            "https://www.fis-ski.com/DB/general/results.html?"
                            f"raceid={race}{'1' if sex == 'M' else '2'}"
                        ),
                        "Sex": sex,
                        "City": "Example",
                        "Country": "USA",
                        "Distance": "Giant Slalom",
                        "Period": "1",
                        "Championship": "0",
                        "Race": race,
                    }
                )
        for gender in ("men", "ladies"):
            with (root / f"startlist_races_{gender}.csv").open(
                "w", newline="", encoding="utf-8"
            ) as handle:
                writer = csv.DictWriter(
                    handle,
                    fieldnames=(
                        "ID",
                        "Skier",
                        "Race_Date",
                        "Startlist_Source",
                    ),
                )
                writer.writeheader()
                writer.writerow(
                    {
                        "ID": "1",
                        "Skier": "Test Athlete",
                        "Race_Date": "11/28/2026",
                        "Startlist_Source": "generated_pre_race_history",
                    }
                )
            with (root / f"{gender}_chrono.csv").open(
                "w", newline="", encoding="utf-8"
            ) as handle:
                writer = csv.DictWriter(handle, fieldnames=("ID", "Date", "Elo"))
                writer.writeheader()
                writer.writerow({"ID": "1", "Date": "11/01/2026", "Elo": "1500"})
        return root

    def test_aligned_current_inputs_pass(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-ready-") as text:
            result = validate_current_alpine_inputs(
                self.build_inputs(Path(text)),
                self.logical_date,
                2027,
            )
            self.assertEqual("passed", result["status"])
            self.assertEqual(["men", "ladies"], result["genders"])

    def test_stale_startlist_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-stale-") as text:
            root = self.build_inputs(Path(text))
            path = root / "startlist_races_men.csv"
            path.write_text(
                "ID,Skier,Race_Date,Startlist_Source\n"
                "1,Test Athlete,03/25/2026,generated_pre_race_history\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(PipelineCheckError, "stale or mixed"):
                validate_current_alpine_inputs(root, self.logical_date, 2027)

    def test_wrong_season_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-season-") as text:
            root = self.build_inputs(Path(text))
            with self.assertRaisesRegex(PipelineCheckError, "outside FIS season"):
                validate_current_alpine_inputs(root, self.logical_date, 2026)

    def test_non_race_date_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-date-") as text:
            root = self.build_inputs(Path(text))
            with self.assertRaisesRegex(PipelineCheckError, "no Alpine race"):
                validate_current_alpine_inputs(root, date(2026, 11, 29), 2027)

    def test_single_gender_race_requires_only_its_startlist(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-one-gender-") as text:
            root = self.build_inputs(Path(text))
            with (root / "races.csv").open(newline="", encoding="utf-8") as handle:
                races = list(csv.DictReader(handle))
            with (root / "races.csv").open(
                "w", newline="", encoding="utf-8"
            ) as handle:
                writer = csv.DictWriter(handle, fieldnames=ALPINE_RACE_HEADERS)
                writer.writeheader()
                writer.writerow(next(row for row in races if row["Sex"] == "L"))
            (root / "startlist_races_men.csv").unlink()

            result = validate_current_alpine_inputs(
                root,
                self.logical_date,
                2027,
            )

            self.assertEqual(["ladies"], result["genders"])
            self.assertEqual(
                ["generated_pre_race_history"],
                result["startlist_sources"]["ladies"],
            )

    def test_unlabelled_startlist_provenance_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-current-source-") as text:
            root = self.build_inputs(Path(text))
            path = root / "startlist_races_men.csv"
            path.write_text(
                "ID,Skier,Race_Date,Startlist_Source\n"
                "1,Test Athlete,11/28/2026,unknown\n",
                encoding="utf-8",
            )
            with self.assertRaisesRegex(
                PipelineCheckError, "invalid startlist provenance"
            ):
                validate_current_alpine_inputs(root, self.logical_date, 2027)


if __name__ == "__main__":
    unittest.main()
