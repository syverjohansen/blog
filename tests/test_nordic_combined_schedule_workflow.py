from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-schedule-update.yml"


class NordicCombinedScheduleWorkflowTests(unittest.TestCase):
    def test_rollover_promotion_resolves_relative_evidence_from_runtime_root(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        stage = source.split("- name: Stage validated schedule", 1)[1].split(
            "- name: Validate and publish accepted Nordic-combined schedule", 1
        )[0]

        self.assertIn('root="external/ski/elo/python/nordic-combined/polars"', stage)
        self.assertIn('cd "$root"', stage)
        self.assertIn("python3 promote_schedule.py", stage)
        self.assertIn("--accepted-dir excel365", stage)
        self.assertNotIn('python3 "$root/promote_schedule.py"', stage)
        self.assertNotIn('--accepted-dir "$root/excel365"', stage)


if __name__ == "__main__":
    unittest.main()
