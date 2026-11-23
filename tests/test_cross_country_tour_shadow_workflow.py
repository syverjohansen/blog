import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-tour-shadow.yml"


class CrossCountryTourShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]

    def test_is_manual_only_guarded_and_github_hosted(self) -> None:
        self.assertTrue(
            self.source.startswith("name: Cross-country Tour de Ski predictions\n")
        )
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertNotIn("workflow_run:", self.trigger)
        self.assertNotIn("schedule:", self.trigger)
        self.assertIn("permissions:\n  contents: write", self.source)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)
        self.assertIn("token: ${{ secrets.BLOG_AUTOMATION_TOKEN }}", self.source)
        self.assertEqual(1, self.source.count("persist-credentials: false"))

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
            "/cross_country_tour_publication.py",
            "/cross_country_tour_shadow.py",
            "/tds-picks-simulation.R",
            "/content/post/tds-picks/",
            "/data/cross-country/drafts/tds-picks/",
            "/layouts/",
            "/themes/white/",
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
        build_candidate = self.source.index(
            "name: Build publication-disabled Tour candidate"
        )
        render_candidate = self.source.index(
            "name: Render publication-disabled Tour candidate"
        )
        retain = self.source.index("name: Retain complete Tour shadow evidence")
        self.assertLess(build, inspect)
        self.assertLess(inspect, simulate)
        self.assertLess(simulate, validate)
        self.assertLess(validate, lock)
        self.assertLess(lock, build_candidate)
        self.assertLess(build_candidate, render_candidate)
        self.assertLess(render_candidate, retain)
        self.assertIn("if: always()", self.source)
        self.assertIn("retention-days: 30", self.source)
        self.assertIn("run_metadata.json", self.source)
        self.assertIn('"ski_commit": os.environ["SKI_SHA"]', self.source)
        self.assertIn('hugo-version: "0.134.1"', self.source)
        self.assertIn("RENDER_REPORT=$root/reports/render_report.json", self.source)

    def test_waiting_or_no_work_skips_expensive_runtime(self) -> None:
        condition = "if: steps.bundle.outputs.has_work == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 9)
        self.assertIn('"waiting_for_startlists", "no_work"', self.source)
        self.assertIn("No simulation or publication was attempted", self.source)
        self.assertLess(
            self.source.index("name: Inspect Tour bundle decision"),
            self.source.index("r-lib/actions/setup-r@v2"),
        )

    def test_publication_is_narrow_and_follows_all_validation(self) -> None:
        self.assertIn('publication") != {"status": "disabled"}', self.source)
        self.assertIn("Tour shadow passed with publication disabled", self.source)
        render = self.source.index("name: Render publication-disabled Tour candidate")
        install = self.source.index("name: Install validated Tour candidate")
        publish = self.source.index("name: Publish validated Tour predictions")
        retain = self.source.index("name: Retain complete Tour shadow evidence")
        self.assertLess(render, install)
        self.assertLess(install, publish)
        self.assertLess(publish, retain)
        self.assertIn('--as-of "$(date -u +%F)"', self.source)
        self.assertIn("git pull --rebase origin main", self.source)
        self.assertIn("git push origin HEAD:main", self.source)
        self.assertIn(
            '"schema_version": "nordicnumbers.cross-country-tour-publication/v1"',
            self.source,
        )
        for forbidden in (
            "cross_country_weekend_publication",
            "git add .",
            "git add -A",
            "git push --force",
        ):
            self.assertNotIn(forbidden, self.source)

    def test_candidate_is_rendered_installed_and_published_as_exact_files(self) -> None:
        self.assertIn(
            "python3 -m pipelines.cross_country_tour_publication build",
            self.source,
        )
        self.assertIn(
            "python3 -m pipelines.cross_country_tour_publication render",
            self.source,
        )
        self.assertIn('--output-root "$CANDIDATE_ROOT"', self.source)
        self.assertIn('--destination "$SITE_ROOT"', self.source)
        self.assertIn(
            "python3 -m pipelines.cross_country_tour_publication install",
            self.source,
        )
        for path in (
            '"content/post/tds-picks/$date_key.md"',
            '"data/cross-country/drafts/tds-picks/$date_key/men.json"',
            '"data/cross-country/drafts/tds-picks/$date_key/ladies.json"',
            '"data/cross-country/drafts/tds-picks/$date_key/men_position_probabilities.json"',
            '"data/cross-country/drafts/tds-picks/$date_key/ladies_position_probabilities.json"',
        ):
            self.assertIn(path, self.source)
        self.assertIn("$RUNNER_TEMP/cross-country-tour-shadow", self.source)
        self.assertIn("if: always()", self.source)


if __name__ == "__main__":
    unittest.main()
