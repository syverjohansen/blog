from __future__ import annotations

import unittest
from pathlib import Path


BLOG_ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = BLOG_ROOT / ".github/workflows/alpine-site-data-update.yml"


class AlpineSiteDataWorkflowTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_scheduled_and_manual_triggers_are_available(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("schedule:", trigger)
        self.assertIn('cron: "15 0 * * *"', trigger)
        self.assertIn("workflow_dispatch:", trigger)

    def test_master_workflow_is_the_only_daily_alpine_scheduler(self) -> None:
        prediction_workflow = (
            BLOG_ROOT / ".github/workflows/alpine-production-shadow.yml"
        ).read_text(encoding="utf-8")
        prediction_trigger = prediction_workflow.split("on:\n", 1)[1].split(
            "\npermissions:", 1
        )[0]
        self.assertNotIn("schedule:", prediction_trigger)

    def test_chains_results_site_data_and_predictions(self) -> None:
        results = self.source.index("python alpine_results_update.py")
        site_data = self.source.index("python3 -m pipelines.alpine_site_data")
        predictions = self.source.index("gh workflow run alpine-production-shadow.yml")
        self.assertLess(results, site_data)
        self.assertLess(site_data, predictions)

    def test_reads_and_publishes_ski_with_cross_repo_credential(self) -> None:
        self.assertIn("secrets.BLOG_AUTOMATION_TOKEN", self.source)
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("steps.ski.outputs.sha", self.source)
        self.assertIn("men_chrono.csv", self.source)
        self.assertIn("ladies_chrono.csv", self.source)

    def test_generation_and_tests_precede_non_force_publication(self) -> None:
        generation = self.source.index("python3 -m pipelines.alpine_site_data")
        tests = self.source.rindex("python3 -m unittest discover")
        publication = self.source.rindex("git push origin HEAD:main")
        self.assertLess(generation, tests)
        self.assertLess(tests, publication)
        self.assertNotIn("--force", self.source)

    def test_publisher_updates_all_alpine_site_artifacts(self) -> None:
        self.assertIn("--output-root static/python/alpine/excel365", self.source)
        self.assertIn("git add static/python/alpine/excel365", self.source)

    def test_defaults_to_yesterday_and_retains_manual_override(self) -> None:
        self.assertIn("date -u -d 'yesterday' +%F", self.source)
        self.assertIn("REQUESTED_AS_OF", self.source)


if __name__ == "__main__":
    unittest.main()
