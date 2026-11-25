from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/ski-jumping-results-update.yml"


class SkiJumpingResultsWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_follows_schedule_and_has_manual_recovery(self) -> None:
        self.assertIn(
            'workflows: ["Ski-jumping automatic schedule pipeline"]', self.text
        )
        self.assertIn("workflow_dispatch:", self.text)
        self.assertIn("workflow_run.conclusion == 'success'", self.text)

    def test_checkout_is_bounded_to_ski_jumping(self) -> None:
        self.assertIn("/elo/python/skijump/polars/", self.text)
        self.assertNotIn("/elo/python/alpine/", self.text)
        self.assertNotIn("/elo/python/ski/polars/", self.text)

    def test_pipeline_runs_every_guarded_stage_in_order(self) -> None:
        commands = [
            "skijump_results_gap.py",
            "skijump_results_acquire.py",
            "skijump_results_identity.py",
            "skijump_results_elo_replay.py",
            "skijump_results_promote.py",
            "skijump_results_apply.py",
        ]
        positions = [self.text.index(command) for command in commands]
        self.assertEqual(positions, sorted(positions))
        self.assertIn("--fail-on-unresolved", self.text)
        self.assertIn("--expected-report-sha256", self.text)

    def test_no_gap_run_is_successful_and_mutation_steps_are_conditional(self) -> None:
        marker = "if: steps.gap.outputs.count != '0'"
        self.assertGreaterEqual(self.text.count(marker), 6)
        self.assertNotIn("--fail-on-gaps", self.text)

    def test_only_atomic_five_history_set_is_committed(self) -> None:
        staged = self.text.split("git -C external/ski add", 1)[1].split(
            "if ! git", 1
        )[0]
        for name in (
            "men_chrono.csv",
            "ladies_chrono.csv",
            "men_team_chrono.csv",
            "ladies_team_chrono.csv",
            "mixed_team_chrono.csv",
        ):
            self.assertIn(name, staged)
        self.assertNotIn("excel365/races.csv", staged)
        self.assertNotIn("static/", staged)

    def test_failure_evidence_is_retained_without_site_publication(self) -> None:
        self.assertIn("if: always()", self.text)
        self.assertIn("actions/upload-artifact@v4", self.text)
        self.assertIn("retention-days: 30", self.text)
        self.assertNotIn("hugo", self.text.casefold())


if __name__ == "__main__":
    unittest.main()
