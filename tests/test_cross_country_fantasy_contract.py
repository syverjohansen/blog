import unittest

from pipelines.cross_country_fantasy_contract import (
    BUDGET,
    OBJECTIVES,
    SCHEMA_VERSION,
    FantasyContractError,
    build_fantasy_plan,
    validate_team,
)


def prediction(index: int, sex: str) -> dict[str, object]:
    return {
        "canonical_athlete_id": f"canonical-{index}",
        "fis_code": str(3_400_000 + index),
        "canonical_name": f"Athlete {index}",
        "canonical_nation": "NOR",
        "sex": sex,
        "total_points": 100 - index,
        "total_safe": 90 - index,
        "total_upside": 110 - index,
    }


def price(row: dict[str, object], amount: int = 5_000) -> dict[str, object]:
    return {
        "athlete_id": int(str(row["fis_code"])),
        "name": row["canonical_name"],
        "price": amount,
        "country": row["canonical_nation"],
        "gender": "m" if row["sex"] == "M" else "f",
        "active": True,
        "is_team": False,
    }


class CrossCountryFantasyContractTests(unittest.TestCase):
    def setUp(self) -> None:
        self.predictions = [
            *(prediction(index, "M") for index in range(1, 11)),
            *(prediction(index, "L") for index in range(11, 21)),
        ]
        self.prices = [price(row) for row in self.predictions]

    def test_complete_exact_fis_mapping_is_candidate(self) -> None:
        plan = build_fantasy_plan(
            prediction_rows=self.predictions, price_rows=self.prices
        )
        self.assertEqual(SCHEMA_VERSION, plan.schema_version)
        self.assertEqual("candidate", plan.status)
        self.assertEqual(20, plan.priced_prediction_count)
        self.assertEqual(OBJECTIVES, plan.objectives)
        self.assertFalse(plan.weekend_publication_blocked)

    def test_unavailable_api_does_not_block_weekend_post(self) -> None:
        plan = build_fantasy_plan(prediction_rows=self.predictions, price_rows=None)
        self.assertEqual("unavailable", plan.status)
        self.assertFalse(plan.weekend_publication_blocked)
        self.assertEqual(20, len(plan.missing_fis_codes))

    def test_missing_exact_price_blocks_only_fantasy(self) -> None:
        plan = build_fantasy_plan(
            prediction_rows=self.predictions, price_rows=self.prices[:-1]
        )
        self.assertEqual("blocked", plan.status)
        self.assertEqual((self.predictions[-1]["fis_code"],), plan.missing_fis_codes)
        self.assertFalse(plan.weekend_publication_blocked)

    def test_name_match_cannot_replace_missing_fis_code(self) -> None:
        altered = dict(self.prices[-1])
        altered["athlete_id"] = 9_999_999
        prices = [*self.prices[:-1], altered]
        plan = build_fantasy_plan(prediction_rows=self.predictions, price_rows=prices)
        self.assertEqual("blocked", plan.status)
        self.assertIn(self.predictions[-1]["fis_code"], plan.missing_fis_codes)

    def test_price_source_cannot_silently_change(self) -> None:
        with self.assertRaisesRegex(FantasyContractError, "unsupported price source"):
            build_fantasy_plan(
                prediction_rows=self.predictions,
                price_rows=self.prices,
                price_source="https://example.test/athletes",
            )

    def test_all_three_scores_are_required(self) -> None:
        del self.predictions[0]["total_safe"]
        with self.assertRaisesRegex(FantasyContractError, "total_safe"):
            build_fantasy_plan(prediction_rows=self.predictions, price_rows=self.prices)

    def test_duplicate_fis_code_fails_closed(self) -> None:
        self.predictions[-1]["fis_code"] = self.predictions[0]["fis_code"]
        with self.assertRaisesRegex(FantasyContractError, "duplicate"):
            build_fantasy_plan(prediction_rows=self.predictions, price_rows=self.prices)

    def test_valid_optimizer_output(self) -> None:
        rows = []
        for item, price_row in zip(
            self.predictions[:8] + self.predictions[10:18],
            self.prices[:8] + self.prices[10:18],
            strict=True,
        ):
            rows.append(
                {
                    "fis_code": item["fis_code"],
                    "sex": item["sex"],
                    "price": price_row["price"],
                    "objective_points": item["total_points"],
                }
            )
        report = validate_team(objective="normal", team_rows=rows)
        self.assertEqual("valid", report["status"])
        self.assertEqual(8, report["men"])
        self.assertEqual(8, report["ladies"])
        self.assertLessEqual(report["total_price"], BUDGET)

    def test_budget_and_balance_are_enforced(self) -> None:
        rows = [
            {
                "fis_code": str(3_500_000 + index),
                "sex": "M" if index < 9 else "L",
                "price": 7_000,
                "objective_points": 10,
            }
            for index in range(16)
        ]
        with self.assertRaisesRegex(FantasyContractError, "8 men and 8 ladies"):
            validate_team(objective="normal", team_rows=rows)
        rows[8]["sex"] = "L"
        with self.assertRaisesRegex(FantasyContractError, "exceeds budget"):
            validate_team(objective="normal", team_rows=rows)


if __name__ == "__main__":
    unittest.main()
