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
        self.assertIn("workflow_dispatch:", trigger)

    def test_reads_accepted_ski_histories_at_an_exact_commit(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.source)
        self.assertIn("steps.ski.outputs.sha", self.source)
        self.assertIn("men_chrono.csv", self.source)
        self.assertIn("ladies_chrono.csv", self.source)

    def test_generation_and_tests_precede_non_force_publication(self) -> None:
        generation = self.source.index("python3 -m pipelines.alpine_site_data")
        tests = self.source.index("python3 -m unittest discover")
        publication = self.source.index("git push origin HEAD:main")
        self.assertLess(generation, tests)
        self.assertLess(tests, publication)
        self.assertNotIn("--force", self.source)

    def test_publisher_updates_all_alpine_site_artifacts(self) -> None:
        self.assertIn("--output-root static/python/alpine/excel365", self.source)
        self.assertIn("git add static/python/alpine/excel365", self.source)


if __name__ == "__main__":
    unittest.main()
