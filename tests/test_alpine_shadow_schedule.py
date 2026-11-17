from __future__ import annotations

import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class AlpineShadowScheduleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.wrapper = (
            ROOT / "pipelines/run_alpine_shadow_ubuntu.sh"
        ).read_text(encoding="utf-8")
        cls.master = (ROOT / "master_automation_ubuntu.sh").read_text(
            encoding="utf-8"
        )
        cls.service = (
            ROOT / "ops/systemd/user/nordic-alpine-shadow.service"
        ).read_text(encoding="utf-8")
        cls.timer = (
            ROOT / "ops/systemd/user/nordic-alpine-shadow.timer"
        ).read_text(encoding="utf-8")

    def test_ubuntu_wrapper_is_shadow_only_and_race_day_aware(self) -> None:
        self.assertIn("-m pipelines.alpine_shadow_runner", self.wrapper)
        self.assertIn("--skip-no-race", self.wrapper)
        self.assertIn("--allow-network", self.wrapper)
        self.assertIn("flock -n", self.wrapper)
        self.assertNotIn("git push", self.wrapper)

    def test_retired_master_delegates_without_legacy_side_effects(self) -> None:
        self.assertIn("is retired", self.master)
        self.assertIn("run_alpine_shadow_ubuntu.sh", self.master)
        self.assertNotIn("git pull", self.master)
        self.assertNotIn("git push", self.master)
        self.assertNotIn("predict_script_ubuntu.sh", self.master)

    def test_systemd_timer_uses_the_same_publication_disabled_entrypoint(self) -> None:
        self.assertIn("run_alpine_shadow_ubuntu.sh", self.service)
        self.assertIn("OnCalendar=*-*-* 00:15:00 UTC", self.timer)
        self.assertIn("Persistent=true", self.timer)


if __name__ == "__main__":
    unittest.main()
