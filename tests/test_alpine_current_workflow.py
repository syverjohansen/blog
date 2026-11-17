from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/alpine-current-season-smoke.yml"


class AlpineCurrentWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_is_manual_and_publication_disabled(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertNotIn("push:", trigger)
        self.assertIn("alpine-current-smoke", self.source)
        self.assertIn('publication.get("status") != "disabled"', self.source)

    def test_current_ref_is_resolved_to_an_exact_recorded_commit(self) -> None:
        self.assertIn('repos/syverjohansen/ski/commits/$SKI_REF', self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertIn('--ski-commit-sha "$SKI_SHA"', self.source)
        self.assertIn("--ski-input-root", self.source)

    def test_ski_shadow_scraper_builds_the_handoff(self) -> None:
        self.assertIn("startlist-scrape-races.py shadow", self.source)
        self.assertIn("--allow-network", self.source)
        self.assertIn("external/ski-input/reports/replay.json", self.source)
        self.assertIn("generated_pre_race_history", self.source)
        self.assertNotIn("raw.githubusercontent.com", self.source)

    def test_current_smoke_has_no_historical_baseline_comparison(self) -> None:
        self.assertIn("--expected-season", self.source)
        self.assertNotIn("compare_with_accepted_output", self.source)
        self.assertNotIn("git push", self.source)
        self.assertNotIn("aws s3", self.source)


if __name__ == "__main__":
    unittest.main()
