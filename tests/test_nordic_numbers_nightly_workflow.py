from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-numbers-nightly.yml"


class NordicNumbersNightlyWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_one_schedule_dispatches_all_sport_roots_independently(self) -> None:
        self.assertIn('cron: "15 0 * * *"', self.source)
        self.assertIn("fail-fast: false", self.source)
        for workflow in (
            "alpine-site-data-update.yml",
            "cross-country-site-data-update.yml",
            "biathlon-site-data-update.yml",
            "ski-jumping-schedule-update.yml",
            "nordic-combined-schedule-update.yml",
        ):
            self.assertIn(f"workflow: {workflow}", self.source)
        self.assertIn('gh workflow run "$WORKFLOW"', self.source)
        self.assertIn("actions: write", self.source)

    def test_former_roots_do_not_retain_cross_sport_or_duplicate_schedules(self) -> None:
        for name in (
            "alpine-site-data-update.yml",
            "cross-country-site-data-update.yml",
            "biathlon-site-data-update.yml",
            "ski-jumping-schedule-update.yml",
            "nordic-combined-schedule-update.yml",
        ):
            source = (ROOT / ".github/workflows" / name).read_text(encoding="utf-8")
            trigger = source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
            self.assertIn("workflow_dispatch:", trigger)
            self.assertNotIn("schedule:", trigger)
            self.assertNotIn("workflow_run:", trigger)

    def test_startlist_pollers_remain_scheduled_and_publication_enabled(self) -> None:
        for name in (
            "biathlon-predictions.yml",
            "ski-jumping-prediction-shadow.yml",
            "nordic-combined-prediction-shadow.yml",
        ):
            source = (ROOT / ".github/workflows" / name).read_text(encoding="utf-8")
            trigger = source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
            self.assertIn("schedule:", trigger)
        nordic = (
            ROOT / ".github/workflows/nordic-combined-prediction-shadow.yml"
        ).read_text(encoding="utf-8")
        jumping = (
            ROOT / ".github/workflows/ski-jumping-prediction-shadow.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("github.event_name == 'schedule'", nordic)
        self.assertIn("github.event_name == 'schedule'", jumping)

    def test_each_sport_has_a_guarded_publication_path(self) -> None:
        workflows = ROOT / ".github/workflows"
        alpine_site = (workflows / "alpine-site-data-update.yml").read_text()
        alpine_prediction = (workflows / "alpine-production-shadow.yml").read_text()
        cross_individual = (
            workflows / "cross-country-individual-predictions.yml"
        ).read_text()
        cross_weekend = (
            workflows / "cross-country-weekend-shadow.yml"
        ).read_text()
        biathlon_site = (workflows / "biathlon-site-data-update.yml").read_text()
        biathlon_prediction = (workflows / "biathlon-predictions.yml").read_text()
        jumping_site = (
            workflows / "ski-jumping-site-data-update.yml"
        ).read_text()
        jumping_prediction = (
            workflows / "ski-jumping-prediction-shadow.yml"
        ).read_text()
        nordic_site = (
            workflows / "nordic-combined-site-data-update.yml"
        ).read_text()
        nordic_prediction = (
            workflows / "nordic-combined-prediction-shadow.yml"
        ).read_text()

        self.assertIn("-f publish_predictions=true", alpine_site)
        self.assertIn("Publish validated prediction post", alpine_prediction)
        self.assertIn("git push origin HEAD:main", cross_individual)
        self.assertIn("Publish validated weekend predictions", cross_weekend)
        self.assertIn("gh workflow run biathlon-predictions.yml", biathlon_site)
        self.assertIn("Publish validated Biathlon predictions", biathlon_prediction)
        self.assertIn("-f publish_predictions=true", jumping_site)
        self.assertIn("Publish validated ski-jumping predictions", jumping_prediction)
        self.assertIn("-f publish_predictions=true", nordic_site)
        self.assertIn("Publish validated Nordic Combined predictions", nordic_prediction)


if __name__ == "__main__":
    unittest.main()
