from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/biathlon-site-data-update.yml"


class BiathlonSiteDataWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_follows_result_ingestion_daily_and_has_manual_recovery(self) -> None:
        self.assertIn('cron: "17 21 * 11,12,1,2,3 *"', self.text)
        self.assertIn('cron: "17 21 1 5 *"', self.text)
        self.assertIn("workflow_dispatch:", self.text)
        self.assertNotIn("workflow_run:", self.text)

    def test_checkout_is_pinned_and_bounded_to_required_histories(self) -> None:
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", self.text)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.text)
        for path in (
            "/elo/python/biathlon/polars/relay/excel365/men_chrono.csv",
            "/elo/python/biathlon/polars/relay/excel365/ladies_chrono.csv",
            "/ranks/biathlon/excel365/men_scrape_update.csv",
            "/ranks/biathlon/excel365/ladies_scrape_update.csv",
            "/elo/python/biathlon/polars/biathlon_season_close.py",
            "/elo/python/biathlon/polars/excel365/races.csv",
        ):
            self.assertIn(path, self.text)
        self.assertNotIn("/elo/python/alpine/", self.text)
        self.assertNotIn("/elo/python/ski/polars/", self.text)

    def test_may_first_close_is_guarded_and_evidence_is_retained(self) -> None:
        close = self.text.index("name: Verify guarded Biathlon season close")
        generation = self.text.index("python3 -m pipelines.biathlon_site_data")
        self.assertLess(close, generation)
        self.assertIn('close_date="$SEASON-05-01"', self.text)
        self.assertIn("python3 \"$root/biathlon_season_close.py\"", self.text)
        self.assertIn("biathlon-season-close-${{ github.run_id }}", self.text)

    def test_generation_is_isolated_then_validated(self) -> None:
        generation = self.text.index("python3 -m pipelines.biathlon_site_data")
        contract = self.text.index("name: Validate candidate contract")
        materialization = self.text.index("name: Materialize candidate into owned site root")
        tests = self.text.index("python3 -m unittest discover")
        render = self.text.index("hugo --minify")
        publication = self.text.index("git push origin HEAD:main")
        self.assertLess(generation, contract)
        self.assertLess(contract, materialization)
        self.assertLess(materialization, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publication)
        self.assertIn("$RUNNER_TEMP/biathlon-site-candidate", self.text)
        self.assertIn("hugo --minify --renderToMemory", self.text)

    def test_current_season_is_regenerated_without_deleting_history(self) -> None:
        self.assertIn('rm -rf "$artifact_root/M/$SEASON"', self.text)
        self.assertIn('"$artifact_root/L/$SEASON"', self.text)
        self.assertNotIn('rsync --delete', self.text)
        self.assertIn('rsync -a "$CANDIDATE_ROOT/" "$artifact_root/"', self.text)

    def test_publication_is_strictly_bounded_to_owned_root(self) -> None:
        root = "static/python/biathlon/excel365"
        self.assertIn(f'artifact_root="{root}"', self.text)
        self.assertIn('git add -- "$artifact_root"', self.text)
        self.assertIn("git diff --cached --name-only", self.text)
        self.assertIn("Refusing to publish staged files outside", self.text)
        self.assertNotIn("git add .", self.text)
        self.assertNotIn("--force", self.text)

    def test_publication_rebases_and_retains_evidence(self) -> None:
        self.assertIn("git pull --rebase origin main", self.text)
        self.assertIn("if: always()", self.text)
        self.assertIn("actions/upload-artifact@v4", self.text)
        self.assertIn("retention-days: 30", self.text)

    def test_generation_receives_rankings_and_deterministic_as_of(self) -> None:
        self.assertIn("--men-ranking-history", self.text)
        self.assertIn("--ladies-ranking-history", self.text)
        self.assertIn("--as-of", self.text)
        self.assertIn("date -u +%F", self.text)


if __name__ == "__main__":
    unittest.main()

