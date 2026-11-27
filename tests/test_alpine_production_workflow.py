from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/alpine-production-shadow.yml"


class AlpineProductionWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split(
            "\npermissions:", 1
        )[0]

    def test_workflow_is_dispatched_by_the_daily_orchestrator_or_manually(self) -> None:
        self.assertNotIn("schedule:", self.trigger)
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("runs-on: self-hosted", self.source)

    def test_workflow_runs_the_production_shadow_without_publication(self) -> None:
        self.assertIn("-m pipelines.alpine_shadow_runner", self.source)
        self.assertIn("--skip-no-race", self.source)
        self.assertIn("--allow-network", self.source)
        self.assertIn('get("status") != "disabled"', self.source)
        shadow = self.source.split("  publish_prediction_post:", 1)[0]
        self.assertNotIn("git push", shadow)
        self.assertNotIn("contents: write", shadow)

    def test_workflow_pins_ski_and_retains_complete_evidence(self) -> None:
        self.assertIn('repos/syverjohansen/ski/commits/$SKI_REF', self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertIn("printf '/external/", self.source)
        self.assertIn('status --porcelain)', self.source)
        self.assertIn("actions/upload-artifact@v4", self.source)
        self.assertIn("alpine-production-shadow/runs/", self.source)
        self.assertIn("retention-days: 30", self.source)
        self.assertIn("alpine_results_identity.py", self.source)
        self.assertIn("fis_identity_aliases.csv", self.source)

    def test_production_predictions_use_stable_simulation_count(self) -> None:
        self.assertIn("default: 10000", self.source)
        self.assertIn("inputs.n_simulations || 10000", self.source)

    def test_quiet_days_avoid_expensive_r_and_hugo_setup(self) -> None:
        condition = "if: steps.prepare.outputs.has_race == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 4)
        self.assertIn("has_race={str(has_race).lower()}", self.source)

    def test_publication_is_opt_in_direct_and_fail_closed(self) -> None:
        candidate = self.source.split("  publish_prediction_post:", 1)[1]
        self.assertIn("inputs.publish_predictions == true", candidate)
        self.assertIn("needs.shadow.outputs.has_race == 'true'", candidate)
        self.assertIn("timeout-minutes: 30", candidate)
        self.assertIn("contents: write", candidate)
        self.assertNotIn("pull-requests: write", candidate)
        self.assertIn("pipelines.alpine_publication_candidate", candidate)
        self.assertIn("gh workflow run combined-race-picks.yml", candidate)
        self.assertIn("actions: write", candidate)
        self.assertIn('"data/alpine/drafts/race-picks/$DATE_KEY"', candidate)
        self.assertIn(
            '"race-picks/fragments/$DATE_KEY/alpine.md"', candidate
        )
        self.assertIn("git push origin HEAD:main", candidate)
        self.assertNotIn("git add .", candidate)
        self.assertNotIn("--force", candidate)
        self.assertNotIn("gh pr create", candidate)
        self.assertNotIn("gh pr merge", candidate)

        validation = candidate.index("pipelines.alpine_publication_candidate")
        publication = candidate.index("git push origin HEAD:main")
        aggregation = candidate.index("gh workflow run combined-race-picks.yml")
        self.assertLess(validation, publication)
        self.assertLess(publication, aggregation)

    def test_obsolete_host_automation_is_removed(self) -> None:
        self.assertFalse((ROOT / "master_automation_ubuntu.sh").exists())
        self.assertFalse(
            (ROOT / ".github/workflows/self-hosted-automation.yml").exists()
        )
        self.assertFalse((ROOT / ".github/workflows/blog-automation.yml").exists())
        self.assertFalse(
            any(path.is_file() for path in (ROOT / "ops/systemd").rglob("*"))
        )


if __name__ == "__main__":
    unittest.main()
