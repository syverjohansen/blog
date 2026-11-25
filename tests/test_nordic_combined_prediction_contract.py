from __future__ import annotations

import csv
from datetime import date
import json
import tempfile
import unittest
from pathlib import Path

from pipelines.nordic_combined_prediction_contract import (
    PredictionContractError,
    build_plan,
    main,
)


ROOT = Path(__file__).resolve().parents[1]
FIELDS = [
    "Date", "Final_Date", "Startlist", "Sex", "City", "Country",
    "Distance", "RaceType", "Period", "Elevation", "Championship", "Race",
]


def row(*, sex: str, race_type: str, race: int, value: str = "02/10/2026") -> dict[str, object]:
    return {
        "Date": value,
        "Final_Date": value,
        "Startlist": f"https://fis.example/results?raceid={race}",
        "Sex": sex,
        "City": "Predazzo",
        "Country": "Italy",
        "Distance": "10",
        "RaceType": race_type,
        "Period": "4",
        "Elevation": "1000",
        "Championship": "1",
        "Race": race,
    }


def write_schedule(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


class NordicCombinedPredictionContractTests(unittest.TestCase):
    def test_plans_individual_same_sex_team_and_mixed_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            write_schedule(
                schedule,
                [
                    row(sex="M", race_type="Individual", race=1),
                    row(sex="L", race_type="Individual Compact", race=2),
                    row(sex="M", race_type="Team", race=3),
                    row(sex="L", race_type="Team", race=4),
                    row(sex="M", race_type="Team Sprint", race=5),
                    row(sex="L", race_type="Team Sprint", race=6),
                    row(sex="Mixed", race_type="Team", race=7),
                    row(sex="Mixed", race_type="Team Sprint", race=8),
                ],
            )
            plan = build_plan(schedule, date(2026, 2, 10))

        self.assertEqual("ready_for_shadow_implementation", plan["status"])
        self.assertFalse(plan["publication_enabled"])
        self.assertEqual(8, len(plan["events"]))
        self.assertEqual(
            {
                "men_position_probabilities.xlsx",
                "ladies_position_probabilities.xlsx",
                "men_team_position_probabilities.xlsx",
                "ladies_team_position_probabilities.xlsx",
                "men_team_sprint_position_probabilities.xlsx",
                "ladies_team_sprint_position_probabilities.xlsx",
                "mixed_team_position_probabilities.xlsx",
                "mixed_team_sprint_position_probabilities.xlsx",
            },
            set(plan["model"]["workbooks"]),
        )
        self.assertEqual(
            "content/post/race-picks/20260210.md", plan["candidate"]["post"]
        )
        self.assertEqual([], plan["candidate"]["optional_legacy_points_files"])
        self.assertEqual(
            "2026-02-10", plan["model"]["environment"]["NORDIC_LOGICAL_DATE"]
        )
        self.assertEqual(
            "false", plan["model"]["environment"]["NORDIC_LOAD_ENV"]
        )
        self.assertIn(
            "NORDIC_TEAM_INPUT_ROOT", plan["model"]["environment"]
        )

    def test_multiple_individual_races_expand_converter_outputs(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            write_schedule(
                schedule,
                [
                    row(sex="M", race_type="Individual", race=1),
                    row(sex="M", race_type="Mass Start", race=2),
                ],
            )
            plan = build_plan(schedule, date(2026, 2, 10))

        self.assertEqual(
            [
                "men_position_probabilities_Men_Race_1.json",
                "men_position_probabilities_Men_Race_2.json",
            ],
            plan["conversion"]["json_outputs"],
        )

    def test_multiple_same_category_team_events_block(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            write_schedule(
                schedule,
                [
                    row(sex="M", race_type="Team Sprint", race=1),
                    row(sex="M", race_type="Team Sprint", race=2),
                ],
            )
            plan = build_plan(schedule, date(2026, 2, 10))

        self.assertEqual("blocked", plan["status"])
        self.assertIn("retains only one", plan["blockers"][0])

    def test_no_races_is_a_successful_no_op(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            write_schedule(schedule, [row(sex="M", race_type="Individual", race=1)])
            plan = build_plan(schedule, date(2026, 2, 11))

        self.assertEqual("no_races", plan["status"])
        self.assertEqual([], plan["events"])
        self.assertEqual([], plan["model"]["workbooks"])

    def test_invalid_or_duplicate_schedule_rows_fail_closed(self) -> None:
        cases = (
            [row(sex="X", race_type="Individual", race=1)],
            [row(sex="Mixed", race_type="Individual", race=1)],
            [
                row(sex="M", race_type="Individual", race=1),
                row(sex="M", race_type="Sprint", race=1),
            ],
        )
        for rows in cases:
            with self.subTest(rows=rows), tempfile.TemporaryDirectory() as directory:
                schedule = Path(directory) / "races.csv"
                write_schedule(schedule, rows)
                with self.assertRaises(PredictionContractError):
                    build_plan(schedule, date(2026, 2, 10))

    def test_cli_writes_atomic_publication_disabled_plan(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            schedule = root / "races.csv"
            output = root / "report/plan.json"
            write_schedule(schedule, [row(sex="L", race_type="Individual", race=7)])
            result = main(
                (
                    "--schedule", str(schedule),
                    "--logical-date", "2026-02-10",
                    "--output", str(output),
                )
            )
            document = json.loads(output.read_text(encoding="utf-8"))

        self.assertEqual(0, result)
        self.assertFalse(document["publication_enabled"])
        self.assertEqual("ladies", document["events"][0]["category"])

    def test_inventory_pins_existing_blog_components_and_legacy_gaps(self) -> None:
        inventory = json.loads(
            (ROOT / "pipelines/nordic_combined_prediction_inventory.json").read_text(
                encoding="utf-8"
            )
        )
        self.assertEqual(
            "nordicnumbers.nordic-combined-prediction-inventory/v1",
            inventory["schema_version"],
        )
        self.assertEqual("runtime-parameterized", inventory["status"])
        for key in ("model", "model_parameters", "logging", "converter"):
            self.assertTrue((ROOT / inventory["repositories"]["blog"][key]).is_file())
        gaps = " ".join(inventory["known_gaps"])
        self.assertIn("mutable home-directory worktrees", gaps)
        self.assertIn("not yet normalized", gaps)
        self.assertIn("probability workbooks only", gaps)


if __name__ == "__main__":
    unittest.main()
