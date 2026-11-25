from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-schedule-update.yml"


class NordicCombinedScheduleWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_is_chained_and_manually_recoverable(self) -> None:
        self.assertIn('workflows: ["Nordic Numbers weekly recap"]', self.text)
        self.assertIn("workflow_dispatch:", self.text)
        self.assertIn("workflow_run.conclusion == 'success'", self.text)

    def test_checkout_is_bounded_to_nordic_combined_runtime(self) -> None:
        self.assertIn("/elo/python/nordic-combined/polars/", self.text)
        self.assertNotIn("/elo/python/alpine/", self.text)
        self.assertNotIn("/elo/python/ski/polars/", self.text)
        self.assertNotIn("/elo/python/skijump/polars/", self.text)

    def test_october_rollover_and_explicit_promotion_are_required(self) -> None:
        self.assertIn('rollover_date="$accepted_season-10-01"', self.text)
        self.assertIn('promotion" == "approval_required"', self.text)
        self.assertIn("--expected-report-sha256", self.text)
        self.assertIn("--approve-season", self.text)

    def test_only_accepted_schedule_pair_is_staged(self) -> None:
        marker = "git -C external/ski add"
        staged = self.text.split(marker, 1)[1].split("if ! git", 1)[0]
        self.assertIn("excel365/races.csv", staged)
        self.assertIn("excel365/weekends.csv", staged)
        self.assertNotIn("chrono.csv", staged)
        self.assertNotIn("static/", staged)

    def test_candidate_evidence_is_retained_without_site_publication(self) -> None:
        self.assertIn("actions/upload-artifact@v4", self.text)
        self.assertIn("retention-days: 30", self.text)
        self.assertNotIn("hugo", self.text.casefold())
        self.assertNotIn("contents: read", self.text)


if __name__ == "__main__":
    unittest.main()
