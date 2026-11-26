from __future__ import annotations

import unittest
from pathlib import Path


BLOG_ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = BLOG_ROOT / ".github/workflows/alpine-site-data-update.yml"
COORDINATOR = BLOG_ROOT / ".github/workflows/nordic-numbers-nightly.yml"


class AlpineSiteDataWorkflowTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_manual_trigger_is_dispatched_by_the_nightly_coordinator(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        coordinator = COORDINATOR.read_text(encoding="utf-8")
        self.assertIn('cron: "15 0 * * *"', coordinator)
        self.assertIn("workflow: alpine-site-data-update.yml", coordinator)

    def test_master_workflow_is_the_only_daily_alpine_scheduler(self) -> None:
        prediction_workflow = (
            BLOG_ROOT / ".github/workflows/alpine-production-shadow.yml"
        ).read_text(encoding="utf-8")
        prediction_trigger = prediction_workflow.split("on:\n", 1)[1].split(
            "\npermissions:", 1
        )[0]
        self.assertNotIn("schedule:", prediction_trigger)

    def test_chains_results_site_data_and_predictions(self) -> None:
        schedule = self.source.index("python scrape_schedule_shadow.py")
        results = self.source.index("python alpine_results_update.py")
        season_close = self.source.index("alpine_season_close.py")
        site_data = self.source.index("python3 -m pipelines.alpine_site_data")
        predictions = self.source.index("gh workflow run alpine-production-shadow.yml")
        self.assertLess(schedule, results)
        self.assertLess(results, season_close)
        self.assertLess(season_close, site_data)
        self.assertLess(results, site_data)
        self.assertLess(site_data, predictions)
        self.assertIn("-f publish_predictions=true", self.source)
        self.assertIn("-f n_simulations=10000", self.source)

    def test_season_close_catches_up_after_may_first_and_is_manual(self) -> None:
        self.assertIn("run_season_close:", self.source)
        self.assertIn('close_date="$SEASON-05-01"', self.source)
        self.assertIn('"$AS_OF" < "$close_date"', self.source)
        self.assertIn('"$FORCE_CLOSE" != "true"', self.source)
        self.assertIn("python3 \"$root/alpine_season_close.py\"", self.source)
        self.assertIn("candidate/men_chrono.csv", self.source)
        self.assertIn("candidate/ladies_chrono.csv", self.source)

    def test_season_close_is_validated_before_ski_publication(self) -> None:
        season_close = self.source.index("alpine_season_close.py")
        validation = self.source.index(
            'python3 -m unittest discover -s "$root"'
        )
        publication = self.source.index("git -C external/ski push origin HEAD:main")
        self.assertLess(season_close, validation)
        self.assertLess(validation, publication)
        self.assertIn('SEASON_CLOSE_STATUS" == "candidate"', self.source)

    def test_schedule_changes_are_validated_and_committed_with_results(self) -> None:
        self.assertIn('promotion" == "eligible"', self.source)
        self.assertIn("candidate/races.csv", self.source)
        self.assertIn("candidate/weekends.csv", self.source)
        self.assertIn("excel365/races.csv", self.source)
        self.assertIn("excel365/weekends.csv", self.source)
        validation = self.source.index(
            'python3 -m unittest discover -s "$root"'
        )
        ski_commit = self.source.index(
            'git -C external/ski commit -m "Update Alpine data'
        )
        self.assertLess(validation, ski_commit)

    def test_season_rollover_is_automatic_from_october_first(self) -> None:
        self.assertIn('rollover_date="$accepted_season-10-01"', self.source)
        self.assertIn("season=$((accepted_season + 1))", self.source)
        self.assertIn("rollover_allowed=true", self.source)
        self.assertIn('promotion" == "approval_required"', self.source)
        self.assertIn("python3 \"$root/promote_schedule.py\"", self.source)
        self.assertIn("--expected-report-sha256", self.source)
        self.assertIn("--approve-season \"$ACQUISITION_SEASON\"", self.source)

    def test_invalid_rollover_retains_the_accepted_schedule(self) -> None:
        self.assertIn("Unsupported schedule transition", self.source)
        self.assertIn("retaining the accepted schedule", self.source)

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

    def test_rankings_use_history_and_immutable_final_standings(self) -> None:
        self.assertIn("/elo/python/standings_contract.py", self.source)
        self.assertIn("/ranks/alpine/excel365/men_scrape_update.csv", self.source)
        self.assertIn("--men-ranking-history", self.source)
        self.assertIn("--finalized-standings-root", self.source)
        self.assertIn("standings_scrape.py", self.source)
        self.assertIn('ranking_standings/$SEASON', self.source)
        close = self.source.index("alpine_season_close.py")
        standings = self.source.index("name: Snapshot finalized official Alpine standings")
        generation = self.source.index("python3 -m pipelines.alpine_site_data")
        self.assertLess(close, standings)
        self.assertLess(standings, generation)

    def test_defaults_to_yesterday_and_retains_manual_override(self) -> None:
        self.assertIn("date -u -d 'yesterday' +%F", self.source)
        self.assertIn("REQUESTED_AS_OF", self.source)


if __name__ == "__main__":
    unittest.main()
