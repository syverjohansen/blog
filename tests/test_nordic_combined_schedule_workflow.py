from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-schedule-update.yml"


class NordicCombinedScheduleWorkflowTests(unittest.TestCase):
    def setUp(self) -> None:
        self.source = WORKFLOW.read_text(encoding="utf-8")

    def test_rollover_promotion_resolves_relative_evidence_from_runtime_root(self) -> None:
        stage = self.source.split("- name: Stage validated schedule", 1)[1].split(
            "- name: Validate and publish accepted Nordic-combined schedule", 1
        )[0]

        self.assertIn('root="external/ski/elo/python/nordic-combined/polars"', stage)
        self.assertIn('cd "$root"', stage)
        self.assertIn("python3 promote_schedule.py", stage)
        self.assertIn("--accepted-dir excel365", stage)
        self.assertNotIn('python3 "$root/promote_schedule.py"', stage)
        self.assertNotIn('--accepted-dir "$root/excel365"', stage)

    def test_runtime_tests_precede_mutation_and_promoted_pair_is_revalidated(self) -> None:
        test_index = self.source.index("- name: Test schedule runtime before mutation")
        stage_index = self.source.index("- name: Stage validated schedule")
        publish_index = self.source.index(
            "- name: Validate and publish accepted Nordic-combined schedule"
        )
        self.assertLess(test_index, stage_index)
        self.assertLess(stage_index, publish_index)

        pre_mutation = self.source[test_index:stage_index]
        self.assertIn("--with requests --with beautifulsoup4", pre_mutation)
        self.assertIn("python -m unittest discover", pre_mutation)

        publish = self.source[publish_index:]
        self.assertIn("validate_season_structure(races, expected_season=season)", publish)
        self.assertIn("validate_relationship(races, weekends)", publish)
        self.assertNotIn("python3 -m unittest discover", publish)


if __name__ == "__main__":
    unittest.main()
