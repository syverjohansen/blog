from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/ski-jumping-results-update.yml"


class SkiJumpingResultsWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_is_explicitly_dispatched_and_has_manual_recovery(self) -> None:
        self.assertIn("workflow_dispatch:", self.text)
        self.assertNotIn("workflow_run:", self.text)
        self.assertIn("gh workflow run ski-jumping-site-data-update.yml", self.text)
        self.assertIn("actions: write", self.text)

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

    def test_season_close_runs_after_results_and_is_manual_recoverable(self) -> None:
        self.assertIn("workflow_dispatch:", self.text)
        self.assertIn("as_of:", self.text)
        results = self.text.index("skijump_results_apply.py")
        close = self.text.index("skijump_season_close.py")
        self.assertLess(results, close)
        self.assertIn('close_date="$SEASON-05-01"', self.text)
        self.assertIn("steps.season_close.outputs.status == 'candidate'", self.text)

    def test_season_close_validates_all_histories_but_mutates_only_individuals(self) -> None:
        close = self.text.split("python3 skijump_season_close.py", 1)[1].split(
            "- name: Validate and publish ski-jumping season close", 1
        )[0]
        for argument in (
            "--men-chrono",
            "--ladies-chrono",
            "--men-team-chrono",
            "--ladies-team-chrono",
            "--mixed-team-chrono",
            "--policy-exclusions",
        ):
            self.assertIn(argument, close)
        publish = self.text.split(
            "- name: Validate and publish ski-jumping season close", 1
        )[1].split("- name: Upload results evidence", 1)[0]
        self.assertIn('pytest -q "$root"/test_skijump_*.py', publish)
        self.assertIn("men_chrono.csv", publish)
        self.assertIn("ladies_chrono.csv", publish)
        self.assertNotIn("men_team_chrono.csv", publish)
        self.assertNotIn("ladies_team_chrono.csv", publish)
        self.assertNotIn("mixed_team_chrono.csv", publish)

    def test_season_close_evidence_is_retained(self) -> None:
        self.assertIn("season_close_root", self.text)
        self.assertIn("season_close_root }}/report.json", self.text)
        self.assertIn("season_close_root }}/candidate/men_chrono.csv", self.text)
        self.assertIn("season_close_root }}/candidate/ladies_chrono.csv", self.text)

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
