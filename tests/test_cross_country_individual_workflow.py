import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-individual-predictions.yml"
SIMULATION = ROOT / "content/post/cross-country/drafts/race-picks-simulation.R"


class CrossCountryIndividualWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.simulation = SIMULATION.read_text(encoding="utf-8")

    def test_workflow_is_dispatched_and_github_hosted(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertNotIn("workflow_run:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertIn("workflow_dispatch:", trigger)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)

    def test_quiet_days_skip_expensive_runtime(self) -> None:
        condition = "if: steps.detect.outputs.has_individual_event == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 12)
        detection = self.source.index("name: Detect today's individual events")
        blog_checkout = self.source.index("name: Check out blog main")
        setup_r = self.source.index("r-lib/actions/setup-r@v2")
        self.assertLess(detection, blog_checkout)
        self.assertLess(detection, setup_r)
        self.assertIn(condition, self.source[blog_checkout:setup_r])

    def test_output_roots_are_initialized_before_github_env_is_reloaded(self) -> None:
        parameters = self.source.split("name: Resolve logical date", 1)[1].split(
            "- name: Detect today's individual events", 1
        )[0]
        self.assertIn('prediction_root="$RUNNER_TEMP/', parameters)
        self.assertIn('log_root="$RUNNER_TEMP/', parameters)
        self.assertIn('mkdir -p "$prediction_root" "$log_root"', parameters)
        self.assertNotIn(
            'mkdir -p "$NORDIC_PREDICTION_OUTPUT_ROOT" "$NORDIC_LOG_ROOT"',
            parameters,
        )

    def test_current_elo_inputs_and_startlists_precede_simulation(self) -> None:
        prepare = self.source.index("cross_country_individual_publication prepare")
        startlist = self.source.index("startlist-scrape-races.py")
        simulation = self.source.index("race-picks-simulation.R")
        conversion = self.source.index("excel_to_hugo_multiple_sheets.py")
        self.assertLess(prepare, startlist)
        self.assertLess(startlist, simulation)
        self.assertLess(simulation, conversion)
        self.assertIn(
            'f"{gender}_chrono_pred.csv"',
            (ROOT / "pipelines/cross_country_individual_publication.py").read_text(
                encoding="utf-8"
            ),
        )

    def test_validation_and_narrow_publication_precede_combined_dispatch(self) -> None:
        build = self.source.index("cross_country_individual_publication build")
        publication = self.source.index("git push origin HEAD:main")
        dispatch = self.source.index("gh workflow run combined-race-picks.yml")
        self.assertLess(build, publication)
        self.assertLess(publication, dispatch)
        self.assertIn('"data/cross-country/drafts/race-picks/$DATE_KEY"', self.source)
        self.assertIn(
            '"data/race-picks/fragments/$DATE_KEY/cross-country-individual.md"',
            self.source,
        )
        self.assertNotIn("git add .", self.source)
        self.assertNotIn("--force", self.source)

    def test_cross_repository_sources_are_exact_and_evidence_is_retained(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("ref: ${{ steps.source.outputs.ski_sha }}", self.source)
        self.assertIn("ref: ${{ steps.source.outputs.blog_sha }}", self.source)
        self.assertIn("Ski source: $SKI_SHA", self.source)
        self.assertIn("actions/upload-artifact@v4", self.source)
        self.assertIn("retention-days: 14", self.source)
        self.assertEqual(self.source.count("fetch-depth: 1"), 2)

    def test_simulation_uses_hosted_paths_and_preserves_fallback_startlists(
        self,
    ) -> None:
        for variable in (
            "NORDIC_BLOG_ROOT",
            "NORDIC_SKI_DATA_ROOT",
            "NORDIC_TEAM_DATA_ROOT",
            "NORDIC_PREDICTION_OUTPUT_ROOT",
            "NORDIC_LOG_ROOT",
            "NORDIC_LOGICAL_DATE",
        ):
            self.assertIn(variable, self.simulation)
        self.assertIn("No published FIS startlist", self.simulation)
        self.assertIn("fallback athletes", self.simulation)


if __name__ == "__main__":
    unittest.main()
