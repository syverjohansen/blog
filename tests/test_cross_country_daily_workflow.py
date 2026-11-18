import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DAILY = ROOT / ".github/workflows/cross-country-site-data-update.yml"
TEAM = ROOT / ".github/workflows/cross-country-team-predictions.yml"


class CrossCountryDailyWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.daily = DAILY.read_text(encoding="utf-8")
        cls.team = TEAM.read_text(encoding="utf-8")

    def test_daily_pipeline_is_chained_and_manual_without_an_independent_cron(self) -> None:
        trigger = self.daily.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_run:", trigger)
        self.assertIn('workflows: ["Alpine Production Shadow"]', trigger)
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertIn("github.event.workflow_run.conclusion == 'success'", self.daily)

    def test_schedule_results_and_site_publication_are_ordered(self) -> None:
        schedule = self.daily.index("name: Build isolated schedule candidate")
        results = self.daily.index("name: Build isolated results candidate")
        publish_ski = self.daily.index("name: Validate and publish accepted ski data")
        site = self.daily.index("name: Generate cross-country site artifacts")
        tests = self.daily.index("name: Run complete blog test suite")
        render = self.daily.index("name: Render site candidate")
        publish_site = self.daily.index("name: Publish validated site artifacts")
        self.assertLess(schedule, results)
        self.assertLess(results, publish_ski)
        self.assertLess(publish_ski, site)
        self.assertLess(site, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publish_site)

    def test_site_work_is_gated_on_a_promoted_results_candidate(self) -> None:
        condition = "if: steps.results_candidate.outputs.status == 'candidate'"
        self.assertGreaterEqual(self.daily.count(condition), 7)
        checkout = self.daily.index("name: Check out blog for site publication")
        generator = self.daily.index("name: Generate cross-country site artifacts")
        self.assertLess(checkout, generator)
        self.assertIn(condition, self.daily[checkout:generator])

    def test_publication_is_narrow_and_cross_repository_source_is_exact(self) -> None:
        self.assertIn("ref: ${{ steps.source.outputs.ski_sha }}", self.daily)
        self.assertIn("ref: ${{ steps.source.outputs.blog_sha }}", self.daily)
        self.assertIn("cross_country_results_update.py", self.daily)
        self.assertIn("pipelines.cross_country_site_data", self.daily)
        self.assertIn("git add static/python/cross-country/excel365", self.daily)
        self.assertNotIn("git add .", self.daily)
        self.assertNotIn("--force", self.daily)

    def test_team_predictions_wait_for_daily_cross_country_data(self) -> None:
        self.assertIn(
            'workflows: ["Cross-country automatic daily pipeline"]', self.team
        )
        self.assertNotIn('workflows: ["Alpine Production Shadow"]', self.team)
        self.assertIn('data_dir="$PWD/data/cross-country', self.team)
        self.assertNotIn('data_dir="$GITHUB_WORKSPACE/data', self.team)


if __name__ == "__main__":
    unittest.main()
