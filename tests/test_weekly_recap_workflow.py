import unittest
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/weekly-recap.yml"


class WeeklyRecapWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")
        cls.document = yaml.safe_load(cls.text)
        cls.job = cls.document[True]["workflow_run"]
        cls.steps = cls.document["jobs"]["recap-and-publish"]["steps"]

    def test_runs_after_daily_prediction_chain_and_is_manually_runnable(self) -> None:
        self.assertEqual(self.job["workflows"], ["Cross-country team predictions"])
        self.assertIn("workflow_dispatch", self.document[True])

    def test_checkout_contains_shared_standings_contract_and_both_sports(self) -> None:
        checkout = next(
            step
            for step in self.steps
            if step.get("name") == "Check out bounded recap inputs"
        )
        sparse = checkout["with"]["sparse-checkout"]
        self.assertIn("/elo/python/standings_contract.py", sparse)
        self.assertIn("/elo/python/alpine/polars/standings_scrape.py", sparse)
        self.assertIn("/elo/python/ski/polars/standings_scrape.py", sparse)

    def test_generation_uses_isolated_outputs_and_validates_before_publish(
        self,
    ) -> None:
        names = [step.get("name") for step in self.steps]
        self.assertLess(
            names.index("Generate and validate per-sport recap artifacts"),
            names.index("Render site candidate"),
        )
        self.assertLess(
            names.index("Render site candidate"),
            names.index("Publish validated weekly recap"),
        )
        self.assertIn("$RUNNER_TEMP/weekly-recap-xlsx", self.text)
        self.assertIn("$RUNNER_TEMP/weekly-recap-json", self.text)
        self.assertIn("validate_sport_artifacts", self.text)
        self.assertNotIn("git add .", self.text)

    def test_source_revisions_are_resolved_to_exact_commits(self) -> None:
        source = next(
            step
            for step in self.steps
            if step.get("name") == "Resolve exact source revisions"
        )
        self.assertIn("ski_sha", source["run"])
        self.assertIn("blog_sha", source["run"])
        self.assertIn("^[0-9a-f]{40}$", source["run"])

    def test_recap_scripts_use_injected_roots_and_logical_date(self) -> None:
        for sport in ("alpine", "cross-country"):
            script = (
                ROOT / "content" / "post" / sport / "drafts" / "race-recap2.R"
            ).read_text(encoding="utf-8")
            self.assertIn("NORDIC_RECAP_DATA_ROOT", script)
            self.assertIn("NORDIC_RECAP_OUTPUT_ROOT", script)
            self.assertIn("NORDIC_LOGICAL_DATE", script)
            self.assertNotIn("as.Date(Sys.time()", script)


if __name__ == "__main__":
    unittest.main()
