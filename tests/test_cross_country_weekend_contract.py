import json
import tempfile
import unittest
from pathlib import Path

from pipelines.cross_country_weekend_contract import (
    SCHEMA_VERSION,
    WeekendContractError,
    plan_scenario,
)

FIXTURES = (
    Path(__file__).parent / "fixtures/cross_country_weekend_contract"
)


class CrossCountryWeekendContractTests(unittest.TestCase):
    def test_deterministic_scenarios(self) -> None:
        for name in (
            "normal_weekend",
            "tour_window",
            "missing_startlists",
            "no_race_week",
        ):
            with self.subTest(name=name):
                path = FIXTURES / f"{name}.json"
                scenario = json.loads(path.read_text(encoding="utf-8"))
                plan = plan_scenario(path).to_dict()
                self.assertEqual(SCHEMA_VERSION, plan["schema_version"])
                self.assertFalse(plan["publication_enabled"])
                self.assertEqual("ski", plan["acquisition_owner"])
                self.assertEqual("blog", plan["publication_owner"])
                for key, value in scenario["expected"].items():
                    self.assertEqual(value, plan[key], key)

    def test_fallback_is_explicit_and_does_not_drop_races(self) -> None:
        plan = plan_scenario(FIXTURES / "missing_startlists.json")
        self.assertEqual("candidate", plan.status)
        self.assertEqual("generated_fallback", plan.startlist_mode)
        self.assertEqual(plan.event_count, plan.individual_event_count)
        self.assertEqual(2, len(plan.missing_startlist_race_ids))

    def test_tour_opening_cannot_enter_normal_weekend_publication(self) -> None:
        plan = plan_scenario(FIXTURES / "tour_window.json")
        self.assertEqual("deferred_specialized", plan.status)
        self.assertIsNone(plan.candidate_post_path)

    def test_team_only_group_remains_owned_by_team_workflow(self) -> None:
        scenario_path = FIXTURES / "normal_weekend.json"
        scenario = json.loads(scenario_path.read_text(encoding="utf-8"))
        for row in scenario["schedule"]:
            row["Distance"] = "Rel"
        temporary = self.enterContext(tempfile.TemporaryDirectory())
        path = Path(temporary) / "team-only.json"
        path.write_text(json.dumps(scenario), encoding="utf-8")
        plan = plan_scenario(path)
        self.assertEqual("no_supported_events", plan.status)
        self.assertEqual(0, plan.individual_event_count)
        self.assertEqual(4, plan.team_event_count)
        self.assertIsNone(plan.candidate_post_path)

    def test_duplicate_race_identity_fails_closed(self) -> None:
        scenario_path = FIXTURES / "normal_weekend.json"
        scenario = json.loads(scenario_path.read_text(encoding="utf-8"))
        scenario["schedule"].append(dict(scenario["schedule"][0]))
        temporary = self.enterContext(tempfile.TemporaryDirectory())
        path = Path(temporary) / "duplicate.json"
        path.write_text(json.dumps(scenario), encoding="utf-8")
        with self.assertRaisesRegex(WeekendContractError, "duplicate"):
            plan_scenario(path)


if __name__ == "__main__":
    unittest.main()
