from __future__ import annotations

import unittest
from datetime import date
from pathlib import Path

from pipelines.test_pipeline import (
    PipelineCheckError,
    build_plan,
    inject_failure,
    load_schedule,
    resolve_ski_input,
    validate_fixture_contracts,
    validate_prediction_semantics,
    verify_scenarios,
)


BLOG_ROOT = Path(__file__).resolve().parents[1]
FIXTURE_SCHEDULE = BLOG_ROOT / "tests/pipeline/fixtures/races.csv"


class PipelineOrchestrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.schedule = load_schedule(FIXTURE_SCHEDULE)

    def test_trigger_scenario_matrix(self) -> None:
        results = verify_scenarios(BLOG_ROOT)
        self.assertEqual(6, len(results))
        self.assertTrue(all(result["status"] == "passed" for result in results))

    def test_may_first_rebuild_is_not_suppressed_by_offseason(self) -> None:
        plan = build_plan(date(2026, 5, 1), self.schedule)
        self.assertFalse(plan["facts"]["in_season"])
        self.assertEqual("planned", plan["tasks"]["season_rebuild"]["status"])
        self.assertEqual("planned", plan["tasks"]["website_smoke"]["status"])

    def test_prediction_failure_blocks_website(self) -> None:
        plan = build_plan(date(2026, 3, 24), self.schedule)
        failed = inject_failure(plan, "race_predictions")
        self.assertEqual("failed", failed["tasks"]["race_predictions"]["status"])
        self.assertEqual("blocked", failed["tasks"]["website_smoke"]["status"])

    def test_prediction_contract_fixtures(self) -> None:
        results = validate_fixture_contracts(BLOG_ROOT)
        self.assertEqual([1, 1], [result["rows"] for result in results])
        self.assertTrue(all(result["columns"] == 10 for result in results))

    def test_prediction_probability_semantics(self) -> None:
        fixture_root = (
            BLOG_ROOT
            / "tests/pipeline/fixtures/data/alpine/drafts/race-picks/20260324"
        )
        for gender in ("men", "ladies"):
            result = validate_prediction_semantics(
                fixture_root / f"{gender}_position_probabilities.json"
            )
            self.assertEqual(1, result["rows"])
            self.assertEqual(64, len(result["sha256"]))

    def test_pinned_hosted_inputs_require_full_commit_identity(self) -> None:
        input_root = BLOG_ROOT / "tests/pipeline/fixtures"
        source, state = resolve_ski_input(
            BLOG_ROOT,
            input_root,
            "a" * 40,
        )
        self.assertEqual(input_root, source)
        self.assertEqual("a" * 40, state["commit_sha"])
        self.assertEqual("pinned-input-files", state["source"])
        with self.assertRaisesRegex(PipelineCheckError, "40-character"):
            resolve_ski_input(BLOG_ROOT, input_root, "main")


if __name__ == "__main__":
    unittest.main()
