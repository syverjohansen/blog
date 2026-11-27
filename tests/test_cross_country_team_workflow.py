import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-team-predictions.yml"


class CrossCountryTeamWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_is_explicitly_dispatched_manual_and_github_hosted(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertNotIn("workflow_run:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertIn("workflow_dispatch:", trigger)
        self.assertIn("gh workflow run cross-country-weekend-shadow.yml", self.source)
        self.assertIn("gh workflow run cross-country-tour-shadow.yml", self.source)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)

    def test_quiet_days_skip_expensive_runtime(self) -> None:
        condition = "if: steps.detect.outputs.has_team_event == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 10)
        detection = self.source.index("name: Detect today's team events")
        blog_checkout = self.source.index("name: Check out blog main")
        setup_r = self.source.index("r-lib/actions/setup-r@v2")
        self.assertLess(detection, blog_checkout)
        self.assertLess(detection, setup_r)
        checkout_block = self.source[blog_checkout:setup_r]
        self.assertIn(condition, checkout_block)

    def test_complete_gender_inputs_precede_prediction(self) -> None:
        prepare = self.source.index("cross_country_team_publication prepare")
        relay = self.source.index("startlist_scrape_races_relay.py")
        team_sprint = self.source.index("startlist_scrape_races_team_sprint.py")
        self.assertLess(prepare, relay)
        self.assertLess(prepare, team_sprint)
        self.assertIn("$RUNNER_TEMP/team-event-inputs/relay.csv", self.source)
        self.assertIn("$RUNNER_TEMP/team-event-inputs/team_sprint.csv", self.source)

    def test_validation_and_narrow_publication_precede_combined_dispatch(self) -> None:
        build = self.source.index("cross_country_team_publication build")
        publication = self.source.index("git push origin HEAD:main")
        dispatch = self.source.index("gh workflow run combined-race-picks.yml")
        self.assertLess(build, publication)
        self.assertLess(publication, dispatch)
        self.assertIn('"data/cross-country/drafts/race-picks/$DATE_KEY"', self.source)
        self.assertIn(
            '"data/race-picks/fragments/$DATE_KEY/cross-country-team.md"', self.source
        )
        self.assertNotIn("git add .", self.source)
        self.assertNotIn("--force", self.source)

    def test_cross_repository_source_is_exact_and_evidence_is_retained(self) -> None:
        self.assertIn('repos/syverjohansen/ski/commits/$SKI_REF', self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertIn("Ski source: $SKI_SHA", self.source)
        self.assertIn("actions/upload-artifact@v4", self.source)
        self.assertIn("retention-days: 14", self.source)

    def test_checkouts_are_shallow_and_bounded(self) -> None:
        self.assertEqual(self.source.count("fetch-depth: 1"), 2)
        self.assertNotIn("fetch-depth: 0", self.source)


if __name__ == "__main__":
    unittest.main()
