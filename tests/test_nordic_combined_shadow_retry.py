from __future__ import annotations

import csv
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.nordic_combined_shadow_retry import (
    ACQUISITION_SCHEMA,
    PUBLICATION,
    RetryContractError,
    classify_acquisition,
    resolve_logical_date,
    write_waiting_report,
)


class NordicCombinedShadowRetryTests(unittest.TestCase):
    def write_schedule(self, root: Path) -> Path:
        path = root / "races.csv"
        with path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=["Date", "Race"])
            writer.writeheader()
            writer.writerows(
                [
                    {"Date": "11/27/2026", "Race": 1},
                    {"Date": "11/28/2026", "Race": 2},
                ]
            )
        return path

    def test_scheduled_retry_selects_nearest_race_within_two_days(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = self.write_schedule(Path(directory))
            resolved = resolve_logical_date(
                schedule, event_name="schedule", today=date(2026, 11, 25)
            )
        self.assertEqual(date(2026, 11, 27), resolved)

    def test_manual_date_is_exact_and_no_nearby_race_falls_back_to_today(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = self.write_schedule(Path(directory))
            manual = resolve_logical_date(
                schedule,
                requested="2026-12-04",
                event_name="workflow_dispatch",
                today=date(2026, 11, 25),
            )
            quiet = resolve_logical_date(
                schedule, event_name="schedule", today=date(2026, 11, 20)
            )
        self.assertEqual(date(2026, 12, 4), manual)
        self.assertEqual(date(2026, 11, 20), quiet)

    def test_waiting_is_distinct_from_ready_and_broken_acquisition(self) -> None:
        waiting = {
            "schema_version": ACQUISITION_SCHEMA,
            "status": "waiting",
            "prediction_eligible": False,
            "official_event_count": 0,
            "entrant_row_count": 0,
        }
        ready = {
            "schema_version": ACQUISITION_SCHEMA,
            "status": "ready",
            "prediction_eligible": True,
        }
        self.assertEqual("waiting", classify_acquisition(waiting, step_outcome="failure"))
        self.assertEqual("ready", classify_acquisition(ready, step_outcome="success"))
        with self.assertRaises(RetryContractError):
            classify_acquisition(waiting, step_outcome="success")

    def test_waiting_report_is_skipped_and_publication_disabled(self) -> None:
        acquisition = {
            "schema_version": ACQUISITION_SCHEMA,
            "status": "waiting",
            "prediction_eligible": False,
            "scheduled_event_count": 1,
            "official_event_count": 0,
            "unresolved_event_count": 1,
            "entrant_row_count": 0,
        }
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            output = root / "shadow-report.json"
            report = write_waiting_report(
                output,
                run_id="retry-1",
                logical_date="2026-11-27",
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                acquisition_report_path=root / "acquisition.json",
                acquisition_report=acquisition,
            )
            persisted = json.loads(output.read_text(encoding="utf-8"))
        self.assertEqual("skipped", report["status"])
        self.assertEqual("official_startlist_waiting", report["reason"])
        self.assertEqual(PUBLICATION, report["publication"])
        self.assertEqual(report, persisted)


if __name__ == "__main__":
    unittest.main()
