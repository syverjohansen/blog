import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/weekly-recap.yml"


class WeeklyRecapWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_runs_after_daily_prediction_chain_and_is_manually_runnable(self) -> None:
        self.assertIn('workflows: ["Cross-country team predictions"]', self.text)
        self.assertIn("workflow_dispatch:", self.text)

    def test_checkout_contains_shared_standings_contract_and_all_sports(self) -> None:
        self.assertIn("/elo/python/standings_contract.py", self.text)
        self.assertIn("/elo/python/alpine/polars/standings_scrape.py", self.text)
        self.assertIn("/elo/python/ski/polars/standings_scrape.py", self.text)
        self.assertIn("/elo/python/skijump/polars/standings_scrape.py", self.text)
        self.assertIn("/elo/python/biathlon/polars/standings_scrape.py", self.text)

    def test_generation_uses_isolated_outputs_and_validates_before_publish(
        self,
    ) -> None:
        self.assertLess(
            self.text.index("name: Generate and validate per-sport recap artifacts"),
            self.text.index("name: Render site candidate"),
        )
        self.assertLess(
            self.text.index("name: Render site candidate"),
            self.text.index("name: Publish validated weekly recap"),
        )
        self.assertIn("$RUNNER_TEMP/weekly-recap-xlsx", self.text)
        self.assertIn("$RUNNER_TEMP/weekly-recap-json", self.text)
        self.assertIn("validate_sport_artifacts", self.text)
        self.assertNotIn("git add .", self.text)

    def test_source_revisions_are_resolved_to_exact_commits(self) -> None:
        self.assertIn("ski_sha", self.text)
        self.assertIn("blog_sha", self.text)
        self.assertIn("^[0-9a-f]{40}$", self.text)

    def test_recap_scripts_use_injected_roots_and_logical_date(self) -> None:
        for sport in ("alpine", "cross-country", "skijump", "biathlon"):
            script = (
                ROOT / "content" / "post" / sport / "drafts" / "race-recap2.R"
            ).read_text(encoding="utf-8")
            self.assertIn("NORDIC_RECAP_DATA_ROOT", script)
            self.assertIn("NORDIC_RECAP_OUTPUT_ROOT", script)
            self.assertIn("NORDIC_LOGICAL_DATE", script)
            self.assertNotIn("as.Date(Sys.time()", script)


if __name__ == "__main__":
    unittest.main()
