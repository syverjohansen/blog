import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-tour-shadow.yml"


class CrossCountryTourShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]

    def test_is_manual_only_read_only_and_github_hosted(self) -> None:
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertNotIn("workflow_run:", self.trigger)
        self.assertNotIn("schedule:", self.trigger)
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)
        self.assertNotIn("BLOG_AUTOMATION_TOKEN", self.source)
        self.assertEqual(2, self.source.count("persist-credentials: false"))

    def test_pins_bounded_blog_and_ski_runtimes(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertEqual(2, self.source.count("fetch-depth: 1"))
        for path in (
            "/cross_country_tour_inputs.py",
            "/cross_country_tour_contract.py",
            "/cross_country_weekend_inputs.py",
            "/cross_country_results_identity.py",
            "/schedule_contract.py",
            "/cross_country_tour_shadow.py",
            "/tds-picks-simulation.R",
        ):
            self.assertIn(path, self.source)
        self.assertNotIn("fetch-depth: 0", self.source)

    def test_bundle_precedes_model_and_validation(self) -> None:
        build = self.source.index("name: Build isolated Tour opening bundle")
        inspect = self.source.index("name: Inspect Tour bundle decision")
        simulate = self.source.index("name: Run publication-disabled Tour simulation")
        validate = self.source.index(
            "name: Validate Tour shadow workbooks and manifest"
        )
        lock = self.source.index("name: Enforce publication lock")
        retain = self.source.index("name: Retain complete Tour shadow evidence")
        self.assertLess(build, inspect)
        self.assertLess(inspect, simulate)
        self.assertLess(simulate, validate)
        self.assertLess(validate, lock)
        self.assertLess(lock, retain)
        self.assertIn("if: always()", self.source)
        self.assertIn("retention-days: 30", self.source)
        self.assertIn("run_metadata.json", self.source)
        self.assertIn('"ski_commit": os.environ["SKI_SHA"]', self.source)

    def test_waiting_or_no_work_skips_expensive_runtime(self) -> None:
        condition = "if: steps.bundle.outputs.has_work == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 6)
        self.assertIn('"waiting_for_startlists", "no_work"', self.source)
        self.assertIn("No simulation or publication was attempted", self.source)
        self.assertLess(
            self.source.index("name: Inspect Tour bundle decision"),
            self.source.index("r-lib/actions/setup-r@v2"),
        )

    def test_has_no_publication_or_repository_mutation_path(self) -> None:
        self.assertIn('publication") != {"status": "disabled"}', self.source)
        self.assertIn("Tour shadow passed with publication disabled", self.source)
        for forbidden in (
            "git add",
            "git commit",
            "git push",
            "contents: write",
            "cross_country_weekend_publication",
            "actions-hugo",
        ):
            self.assertNotIn(forbidden, self.source)


if __name__ == "__main__":
    unittest.main()
