from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-results-update.yml"


class NordicCombinedResultsWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_follows_schedule_and_has_manual_recovery(self) -> None:
        self.assertIn(
            'workflows: ["Nordic-combined automatic schedule pipeline"]', self.text
        )
        self.assertIn("workflow_dispatch:", self.text)
        self.assertIn("workflow_run.conclusion == 'success'", self.text)

    def test_checkout_is_bounded_to_nordic_combined(self) -> None:
        self.assertIn("/elo/python/nordic-combined/polars/", self.text)
        self.assertNotIn("/elo/python/alpine/", self.text)
        self.assertNotIn("/elo/python/skijump/", self.text)

    def test_every_guarded_stage_runs_in_order(self) -> None:
        commands = [
            "nordic_combined_results_gap.py",
            "nordic_combined_results_acquire.py",
            "nordic_combined_results_identity.py",
            "nordic_combined_results_replay.py",
            "nordic_combined_results_promote.py",
            "nordic_combined_results_apply.py",
        ]
        positions = [self.text.index(command) for command in commands]
        self.assertEqual(positions, sorted(positions))
        self.assertIn("--fail-on-unresolved", self.text)
        self.assertGreaterEqual(self.text.count("--expected-report-sha256"), 2)

    def test_unsupported_event_types_fail_closed_with_retained_evidence(self) -> None:
        self.assertIn("Enforce supported replay boundary", self.text)
        self.assertIn('(event[1], event[4]) != ("Mixed", "team_sprint")', self.text)
        self.assertIn("unsupported Nordic-combined hosted replay event", self.text)
        self.assertIn("if: always()", self.text)
        self.assertIn("retention-days: 30", self.text)

    def test_no_gap_run_succeeds_and_mutation_is_conditional(self) -> None:
        marker = "if: steps.gap.outputs.count != '0'"
        self.assertGreaterEqual(self.text.count(marker), 7)
        self.assertNotIn("--fail-on-gaps", self.text)

    def test_only_atomic_histories_and_receipt_are_staged(self) -> None:
        staged = self.text.split("git -C external/ski add", 1)[1].split(
            "if ! git", 1
        )[0]
        for name in ("men_chrono.csv", "ladies_chrono.csv", "team_sprint_chrono.csv"):
            self.assertIn(name, staged)
        self.assertIn("results_promotion_receipts", staged)
        self.assertNotIn("excel365/races.csv", staged)
        self.assertNotIn("static/", staged)

    def test_no_site_publication_or_season_close_is_claimed(self) -> None:
        self.assertNotIn("hugo", self.text.casefold())
        self.assertNotIn("season_close", self.text)


if __name__ == "__main__":
    unittest.main()
