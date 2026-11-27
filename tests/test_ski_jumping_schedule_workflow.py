from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/ski-jumping-schedule-update.yml"


class SkiJumpingScheduleWorkflowTests(unittest.TestCase):
    def test_post_stage_schedule_tests_install_scraper_dependencies(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        publish = source.split(
            "- name: Validate and publish accepted ski schedule", 1
        )[1]

        self.assertIn("uv run --isolated --python 3.12", publish)
        self.assertIn("--with requests --with beautifulsoup4", publish)
        self.assertIn("--with pandas --with fuzzywuzzy", publish)
        self.assertIn("python -m unittest discover", publish)
        self.assertNotIn("python3 -m unittest discover", publish)


if __name__ == "__main__":
    unittest.main()
