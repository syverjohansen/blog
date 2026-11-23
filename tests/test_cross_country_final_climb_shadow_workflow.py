import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-final-climb-shadow.yml"


class CrossCountryFinalClimbShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]

    def test_is_manual_publication_disabled_and_github_hosted(self) -> None:
        self.assertTrue(
            self.source.startswith("name: Cross-country Final Climb shadow\n")
        )
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertNotIn("workflow_run:", self.trigger)
        self.assertNotIn("schedule:", self.trigger)
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)
        self.assertNotIn("git push", self.source)

    def test_pins_complete_bounded_blog_and_ski_runtimes(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertEqual(2, self.source.count("fetch-depth: 1"))
        for path in (
            "/cross_country_final_climb_inputs.py",
            "/cross_country_final_climb_contract.py",
            "/cross_country_tour_inputs.py",
            "/cross_country_tour_contract.py",
            "/cross_country_weekend_inputs.py",
            "/cross_country_results_identity.py",
            "/schedule_contract.py",
            "/test_weekends.csv",
            "/cross_country_final_climb_shadow.py",
            "/cross_country_tour_shadow.py",
            "/requirements-snapshot.txt",
            "/final_climb-simulation.R",
        ):
            self.assertIn(path, self.source)
        self.assertNotIn("fetch-depth: 0", self.source)

    def test_bundle_precedes_model_validation_and_lock(self) -> None:
        build = self.source.index("name: Build isolated Final Climb bundle")
        inspect = self.source.index("name: Inspect Final Climb bundle decision")
        simulate = self.source.index(
            "name: Run publication-disabled Final Climb simulation"
        )
        validate = self.source.index(
            "name: Validate Final Climb shadow workbooks and manifest"
        )
        lock = self.source.index("name: Enforce publication lock")
        retain = self.source.index("name: Retain complete Final Climb shadow evidence")
        self.assertLess(build, inspect)
        self.assertLess(inspect, simulate)
        self.assertLess(simulate, validate)
        self.assertLess(validate, lock)
        self.assertLess(lock, retain)
        self.assertIn("if: always()", self.source)
        self.assertIn("retention-days: 30", self.source)

    def test_uses_as_of_window_but_stable_final_climb_target_date(self) -> None:
        self.assertIn("AS_OF_DATE=$AS_OF_DATE_INPUT", self.source)
        self.assertIn('plan.get("final_climb_date")', self.source)
        self.assertIn('"as_of_date": os.environ["AS_OF_DATE_INPUT"]', self.source)
        self.assertIn('"publication": {"status": "disabled"}', self.source)
        self.assertIn('--logical-date "$AS_OF_DATE"', self.source)
        self.assertIn("NORDIC_LOGICAL_DATE: ${{ env.LOGICAL_DATE }}", self.source)

    def test_completed_results_replay_is_explicit_and_provenance_checked(self) -> None:
        self.assertIn("historical_results_replay:", self.trigger)
        self.assertIn("type: boolean", self.trigger)
        self.assertIn("HISTORICAL_REPLAY_INPUT", self.source)
        self.assertIn("--historical-results-replay", self.source)
        self.assertIn(
            'participant_source_mode="historical_results_replay"', self.source
        )
        self.assertIn(
            'report.get("participant_source_mode") != expected_mode', self.source
        )

    def test_waiting_or_no_work_skips_expensive_runtime(self) -> None:
        condition = "if: steps.bundle.outputs.has_work == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 7)
        self.assertIn('"waiting_for_startlists", "no_work"', self.source)
        self.assertIn("No simulation or publication was attempted", self.source)
        self.assertLess(
            self.source.index("name: Inspect Final Climb bundle decision"),
            self.source.index("r-lib/actions/setup-r@v2"),
        )

    def test_records_pinned_provenance_and_exact_output_contract(self) -> None:
        self.assertIn(
            '"schema_version": "nordicnumbers.cross-country-final-climb-run/v1"',
            self.source,
        )
        self.assertIn('"ski_commit": os.environ["SKI_SHA"]', self.source)
        self.assertIn('"blog_commit": os.environ["GITHUB_SHA"]', self.source)
        self.assertIn(
            "python3 -m pipelines.cross_country_final_climb_shadow", self.source
        )
        self.assertIn(
            "Final Climb shadow passed with publication disabled", self.source
        )
        self.assertIn("$RUNNER_TEMP/cross-country-final-climb-shadow", self.source)


if __name__ == "__main__":
    unittest.main()
