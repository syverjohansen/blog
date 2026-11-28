import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DAILY = ROOT / ".github/workflows/cross-country-site-data-update.yml"
TEAM = ROOT / ".github/workflows/cross-country-team-predictions.yml"
INDIVIDUAL = ROOT / ".github/workflows/cross-country-individual-predictions.yml"
FANTASYXC = ROOT / ".github/workflows/cross-country-fantasyxc-evidence.yml"
WEEKEND = ROOT / ".github/workflows/cross-country-weekend-shadow.yml"
TOUR = ROOT / ".github/workflows/cross-country-tour-shadow.yml"
FINAL_CLIMB = ROOT / ".github/workflows/cross-country-final-climb-shadow.yml"


class CrossCountryDailyWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.daily = DAILY.read_text(encoding="utf-8")
        cls.team = TEAM.read_text(encoding="utf-8")
        cls.individual = INDIVIDUAL.read_text(encoding="utf-8")
        cls.fantasyxc = FANTASYXC.read_text(encoding="utf-8")
        cls.weekend = WEEKEND.read_text(encoding="utf-8")
        cls.tour = TOUR.read_text(encoding="utf-8")
        cls.final_climb = FINAL_CLIMB.read_text(encoding="utf-8")

    def test_daily_pipeline_is_independently_dispatched_without_its_own_cron(
        self,
    ) -> None:
        trigger = self.daily.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("workflow_run:", trigger)
        self.assertNotIn("schedule:", trigger)
        coordinator = (
            ROOT / ".github/workflows/nordic-numbers-nightly.yml"
        ).read_text(encoding="utf-8")
        self.assertIn("workflow: cross-country-site-data-update.yml", coordinator)

    def test_schedule_results_and_site_publication_are_ordered(self) -> None:
        schedule = self.daily.index("name: Build isolated schedule candidate")
        results = self.daily.index("name: Build isolated results candidate")
        team_results = self.daily.index("name: Build isolated team-results candidate")
        season_close = self.daily.index("name: Build and stage guarded season close")
        publish_ski = self.daily.index("name: Validate and publish accepted ski data")
        site = self.daily.index("name: Generate cross-country site artifacts")
        tests = self.daily.index("name: Run complete blog test suite")
        render = self.daily.index("name: Render site candidate")
        publish_site = self.daily.index("name: Publish validated site artifacts")
        self.assertLess(schedule, results)
        self.assertLess(results, team_results)
        self.assertLess(team_results, season_close)
        self.assertLess(season_close, publish_ski)
        self.assertLess(results, publish_ski)
        self.assertLess(publish_ski, site)
        self.assertLess(site, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publish_site)

    def test_site_work_is_gated_on_results_close_or_final_standings(self) -> None:
        results_condition = "steps.results_candidate.outputs.status == 'candidate' ||"
        team_results_condition = (
            "steps.team_results_candidate.outputs.status == 'candidate' ||"
        )
        close_condition = "steps.season_close.outputs.status == 'candidate'"
        standings_condition = "steps.standings.outputs.changed == 'true'"
        self.assertGreaterEqual(self.daily.count(results_condition), 6)
        self.assertGreaterEqual(self.daily.count(team_results_condition), 6)
        self.assertGreaterEqual(self.daily.count(close_condition), 6)
        self.assertGreaterEqual(self.daily.count(standings_condition), 6)
        checkout = self.daily.index("name: Check out blog for site publication")
        generator = self.daily.index("name: Generate cross-country site artifacts")
        self.assertLess(checkout, generator)
        self.assertIn(results_condition, self.daily[checkout:generator])
        self.assertIn(team_results_condition, self.daily[checkout:generator])
        self.assertIn(close_condition, self.daily[checkout:generator])
        self.assertIn(standings_condition, self.daily[checkout:generator])

    def test_rankings_include_accepted_team_histories_and_final_standings(self) -> None:
        self.assertIn("/elo/python/standings_contract.py", self.daily)
        self.assertIn("/ranks/ski/excel365/men_scrape_update.csv", self.daily)
        self.assertIn("--men-ranking-history", self.daily)
        self.assertIn("--men-relay-history", self.daily)
        self.assertIn("--ladies-relay-history", self.daily)
        self.assertIn("--team-sprint-history", self.daily)
        self.assertIn("--finalized-standings-root", self.daily)
        self.assertIn("standings_scrape.py", self.daily)
        self.assertIn("ranking_standings/$SEASON", self.daily)

    def test_season_close_is_guarded_idempotent_and_validated(self) -> None:
        self.assertIn("run_season_close:", self.daily)
        self.assertIn('close_date="$SEASON-05-01"', self.daily)
        self.assertIn('"$RUN_DATE" == "$close_date"', self.daily)
        self.assertIn('"$close_as_of" < "$close_date"', self.daily)
        self.assertIn('"$FORCE_CLOSE" != "true"', self.daily)
        self.assertIn("cross_country_season_close.py", self.daily)
        self.assertIn("candidate/men_chrono.csv", self.daily)
        self.assertIn("candidate/ladies_chrono.csv", self.daily)
        close = self.daily.index("cross_country_season_close.py")
        close_report = self.daily.index(
            'report="${{ steps.parameters.outputs.season_close_root }}/report.json"'
        )
        close_command = self.daily[close:close_report]
        self.assertIn(
            '--men-relay-chrono "$root/relay/excel365/men_relay_chrono.csv"',
            close_command,
        )
        self.assertIn(
            '--ladies-relay-chrono "$root/relay/excel365/ladies_relay_chrono.csv"',
            close_command,
        )
        self.assertIn(
            '--team-sprint-chrono "$root/relay/excel365/team_sprint_chrono.csv"',
            close_command,
        )
        validation = self.daily.index("-p 'test_cross_country_season_close.py'")
        publication = self.daily.index("git -C external/ski push origin HEAD:main")
        self.assertLess(close, validation)
        self.assertLess(validation, publication)
        self.assertIn('SEASON_CLOSE_STATUS" == "candidate"', self.daily)

    def test_publication_is_narrow_and_cross_repository_source_is_exact(self) -> None:
        self.assertIn("ref: ${{ steps.source.outputs.ski_sha }}", self.daily)
        self.assertIn("ref: ${{ steps.source.outputs.blog_sha }}", self.daily)
        self.assertIn("cross_country_results_update.py", self.daily)
        self.assertIn("cross_country_team_results_update.py", self.daily)
        self.assertIn("relay/excel365/men_relay_chrono.csv", self.daily)
        self.assertIn("relay/excel365/ladies_relay_chrono.csv", self.daily)
        self.assertIn("relay/excel365/team_sprint_chrono.csv", self.daily)
        self.assertIn("pipelines.cross_country_site_data", self.daily)
        self.assertIn("git add static/python/cross-country/excel365", self.daily)
        self.assertNotIn("git add .", self.daily)
        self.assertNotIn("--force", self.daily)

    def test_first_individual_start_can_bridge_from_accepted_team_history(self) -> None:
        self.assertIn(
            "--men-reference-history relay/excel365/men_chrono.csv", self.daily
        )
        self.assertIn(
            "--ladies-reference-history relay/excel365/ladies_chrono.csv", self.daily
        )

    def test_team_results_are_validated_and_retained_as_daily_evidence(self) -> None:
        team_results = self.daily.index("cross_country_team_results_update.py")
        team_tests = self.daily.index("-p 'test_cross_country_team_*.py'")
        publication = self.daily.index("git -C external/ski push origin HEAD:main")
        self.assertLess(team_results, team_tests)
        self.assertLess(team_tests, publication)
        self.assertIn("team_results_root", self.daily)
        self.assertIn("team_results_root }}/acquisition/report.json", self.daily)
        self.assertIn("team_results_root }}/promotion/report.json", self.daily)

    def test_fis_schedule_dependencies_are_explicit(self) -> None:
        self.assertIn("name: Set up Python runtime", self.daily)
        self.assertIn("uv run --with requests --with beautifulsoup4", self.daily)
        self.assertEqual(self.daily.count("uses: astral-sh/setup-uv@v6"), 1)

    def test_prediction_workflows_form_one_ordered_chain(self) -> None:
        self.assertIn(
            "gh workflow run cross-country-fantasyxc-evidence.yml", self.daily
        )
        self.assertIn(
            "gh workflow run cross-country-individual-predictions.yml",
            self.fantasyxc,
        )
        self.assertNotIn("workflow_run:", self.individual)
        self.assertIn("gh workflow run cross-country-team-predictions.yml", self.individual)
        self.assertIn("gh workflow run cross-country-weekend-shadow.yml", self.team)
        self.assertIn("gh workflow run cross-country-tour-shadow.yml", self.team)
        self.assertIn(
            "gh workflow run cross-country-final-climb-shadow.yml", self.tour
        )
        for source in (
            self.fantasyxc, self.team, self.weekend, self.tour, self.final_climb
        ):
            self.assertNotIn("workflow_run:", source.split("on:\n", 1)[1].split("\npermissions:", 1)[0])
        self.assertNotIn(
            "schedule:", self.tour.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        )
        self.assertNotIn(
            "schedule:",
            self.final_climb.split("on:\n", 1)[1].split("\npermissions:", 1)[0],
        )
        self.assertNotIn('workflows: ["Alpine Production Shadow"]', self.team)
        self.assertIn('data_dir="$PWD/data/cross-country', self.team)
        self.assertNotIn('data_dir="$GITHUB_WORKSPACE/data', self.team)


if __name__ == "__main__":
    unittest.main()
