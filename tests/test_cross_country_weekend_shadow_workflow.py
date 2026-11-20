import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-weekend-shadow.yml"


class CrossCountryWeekendShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]

    def test_workflow_is_manual_github_hosted_and_read_only(self) -> None:
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertNotIn("schedule:", self.trigger)
        self.assertIn("runs-on: ubuntu-latest", self.source)
        self.assertNotIn("self-hosted", self.source)
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertNotIn("git push", self.source)
        self.assertNotIn("contents: write", self.source)

    def test_cross_repository_source_is_resolved_and_bounded(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertEqual(self.source.count("fetch-depth: 1"), 2)
        self.assertNotIn("fetch-depth: 0", self.source)
        self.assertIn("/cross_country_weekend_inputs.py", self.source)
        self.assertIn("/cross_country_weekend_consumer.py", self.source)
        self.assertIn("/pipelines/requirements-snapshot.txt", self.source)

    def test_pipeline_builds_validates_runs_and_retains_evidence(self) -> None:
        build = self.source.index("name: Build isolated weekend input bundle")
        materialize = self.source.index("name: Materialize validated model runtime")
        simulate = self.source.index(
            "name: Run publication-disabled weekend simulation"
        )
        validate = self.source.index("name: Validate shadow workbook and manifest")
        candidate = self.source.index("name: Build publication-disabled Hugo candidate")
        render = self.source.index("name: Render publication-disabled Hugo candidate")
        upload = self.source.index("name: Retain complete shadow evidence")
        self.assertLess(build, materialize)
        self.assertLess(materialize, simulate)
        self.assertLess(simulate, validate)
        self.assertLess(validate, candidate)
        self.assertLess(candidate, render)
        self.assertLess(render, upload)
        self.assertIn("retention-days: 30", self.source)
        self.assertIn("if: always()", self.source)
        self.assertIn("run_metadata.json", self.source)
        self.assertIn('"ski_commit": os.environ["SKI_SHA"]', self.source)

    def test_no_work_date_avoids_expensive_r_runtime(self) -> None:
        condition = "if: steps.bundle.outputs.has_work == 'true'"
        self.assertGreaterEqual(self.source.count(condition), 9)
        inspect = self.source.index("name: Inspect bundle decision")
        setup_r = self.source.index("r-lib/actions/setup-r@v2")
        self.assertLess(inspect, setup_r)

    def test_publication_is_disabled_at_every_boundary(self) -> None:
        self.assertIn('publication") != {"status": "disabled"}', self.source)
        self.assertIn("publication-disabled weekend simulation", self.source)
        self.assertIn("Validate shadow workbook and manifest", self.source)
        self.assertIn('hugo-version: "0.134.1"', self.source)
        self.assertIn("cross_country_weekend_publication render", self.source)
        self.assertIn('--candidate-root "$CANDIDATE_ROOT"', self.source)


if __name__ == "__main__":
    unittest.main()
