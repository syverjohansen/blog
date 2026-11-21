import hashlib
import itertools
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_fantasy_optimizer import (
    PRICE_REPORT_SCHEMA,
    PRICED_INPUT_SCHEMA,
    optimize_fantasy_teams,
)


def write_candidate(root: Path) -> tuple[Path, Path]:
    rows = []
    for sex, offset in (("M", 0), ("L", 10)):
        for local_index in range(10):
            index = offset + local_index
            rows.append(
                {
                    "canonical_athlete_id": str(100 + index),
                    "fis_code": str(3_420_001 + index),
                    "canonical_name": f"Athlete {index}",
                    "canonical_nation": "Norway",
                    "sex": sex,
                    "price": 5_000,
                    "total_points": 100 - local_index,
                    "total_safe": 80 + local_index,
                    "total_upside": 100 + local_index,
                    "races_count": 2,
                    "fantasy_xc_name": f"API {index}",
                    "fantasy_xc_country": "NOR",
                }
            )
    priced = root / "fantasy_priced_inputs.json"
    priced_document = {
        "schema_version": PRICED_INPUT_SCHEMA,
        "logical_date": "2026-11-27",
        "publication": {"status": "disabled"},
        "rows": rows,
    }
    priced.write_text(
        json.dumps(priced_document, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    price_report = root / "price_report.json"
    price_report.write_text(
        json.dumps(
            {
                "schema_version": PRICE_REPORT_SCHEMA,
                "logical_date": "2026-11-27",
                "status": "candidate",
                "publication": {"status": "disabled"},
                "outputs": {
                    priced.name: {
                        "sha256": hashlib.sha256(priced.read_bytes()).hexdigest()
                    }
                },
            }
        ),
        encoding="utf-8",
    )
    return price_report, priced


class CrossCountryFantasyOptimizerTests(unittest.TestCase):
    def test_builds_three_valid_deterministic_teams(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report, priced = write_candidate(root)
            report = optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=priced,
                logical_date=date(2026, 11, 27),
                output_path=root / "fantasy_teams.json",
                report_path=root / "optimizer_report.json",
            )
            self.assertEqual("candidate", report["status"])
            document = json.loads((root / "fantasy_teams.json").read_text())
            self.assertEqual({"normal", "safe", "upside"}, set(document["teams"]))
            for team in document["teams"].values():
                self.assertEqual(16, len(team["rows"]))
                self.assertEqual(8, sum(row["sex"] == "M" for row in team["rows"]))
                self.assertEqual(8, sum(row["sex"] == "L" for row in team["rows"]))
                self.assertLessEqual(team["total_price"], 100_000)
                self.assertEqual("valid", team["validation"]["status"])
            normal_codes = {
                row["fis_code"] for row in document["teams"]["normal"]["rows"]
            }
            safe_codes = {row["fis_code"] for row in document["teams"]["safe"]["rows"]}
            self.assertNotEqual(normal_codes, safe_codes)

    def test_matches_brute_force_optimum_with_variable_prices(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report, priced = write_candidate(root)
            document = json.loads(priced.read_text())
            for index, row in enumerate(document["rows"]):
                row["price"] = 3_000 + (index % 7) * 1_000
                row["total_points"] = 40 + (index * 13) % 61
            priced.write_text(
                json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            report_document = json.loads(price_report.read_text())
            report_document["outputs"][priced.name]["sha256"] = hashlib.sha256(
                priced.read_bytes()
            ).hexdigest()
            price_report.write_text(json.dumps(report_document), encoding="utf-8")

            optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=priced,
                logical_date=date(2026, 11, 27),
                output_path=root / "teams.json",
                report_path=root / "report.json",
            )
            normal = json.loads((root / "teams.json").read_text())["teams"]["normal"]
            pools = {
                sex: [row for row in document["rows"] if row["sex"] == sex]
                for sex in ("M", "L")
            }
            feasible = []
            for men in itertools.combinations(pools["M"], 8):
                for ladies in itertools.combinations(pools["L"], 8):
                    team = men + ladies
                    cost = sum(row["price"] for row in team)
                    if cost <= 100_000:
                        feasible.append(
                            (sum(row["total_points"] for row in team), -cost)
                        )
            expected_points, negative_expected_cost = max(feasible)
            self.assertEqual(expected_points, normal["objective_points"])
            self.assertEqual(-negative_expected_cost, normal["total_price"])

    def test_tie_break_prefers_lower_fis_codes(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report, priced = write_candidate(root)
            document = json.loads(priced.read_text())
            for row in document["rows"]:
                row["total_points"] = 10
            priced.write_text(
                json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            report = json.loads(price_report.read_text())
            report["outputs"][priced.name]["sha256"] = hashlib.sha256(
                priced.read_bytes()
            ).hexdigest()
            price_report.write_text(json.dumps(report), encoding="utf-8")
            optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=priced,
                logical_date=date(2026, 11, 27),
                output_path=root / "teams.json",
                report_path=root / "report.json",
            )
            teams = json.loads((root / "teams.json").read_text())["teams"]
            observed = sorted(row["fis_code"] for row in teams["normal"]["rows"])
            expected = sorted(
                [str(3_420_001 + index) for index in range(8)]
                + [str(3_420_011 + index) for index in range(8)]
            )
            self.assertEqual(expected, observed)

    def test_unavailable_price_report_skips_without_priced_input(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report = root / "price_report.json"
            price_report.write_text(
                json.dumps(
                    {
                        "schema_version": PRICE_REPORT_SCHEMA,
                        "logical_date": "2026-11-27",
                        "status": "unavailable",
                        "reason": "offline",
                        "publication": {"status": "disabled"},
                    }
                ),
                encoding="utf-8",
            )
            report = optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=root / "missing.json",
                logical_date=date(2026, 11, 27),
                output_path=root / "teams.json",
                report_path=root / "report.json",
            )
            self.assertEqual("skipped", report["status"])
            self.assertFalse(report["weekend_publication_blocked"])
            self.assertFalse((root / "teams.json").exists())

    def test_checksum_tamper_blocks_only_fantasy(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report, priced = write_candidate(root)
            priced.write_text(priced.read_text() + " ", encoding="utf-8")
            report = optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=priced,
                logical_date=date(2026, 11, 27),
                output_path=root / "teams.json",
                report_path=root / "report.json",
            )
            self.assertEqual("blocked", report["status"])
            self.assertIn("checksum", report["reason"])
            self.assertFalse(report["weekend_publication_blocked"])

    def test_impossible_budget_blocks_only_fantasy(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            price_report, priced = write_candidate(root)
            document = json.loads(priced.read_text())
            for row in document["rows"]:
                row["price"] = 10_000
            priced.write_text(
                json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
            report_document = json.loads(price_report.read_text())
            report_document["outputs"][priced.name]["sha256"] = hashlib.sha256(
                priced.read_bytes()
            ).hexdigest()
            price_report.write_text(json.dumps(report_document), encoding="utf-8")
            report = optimize_fantasy_teams(
                price_report_path=price_report,
                priced_input_path=priced,
                logical_date=date(2026, 11, 27),
                output_path=root / "teams.json",
                report_path=root / "report.json",
            )
            self.assertEqual("blocked", report["status"])
            self.assertFalse(report["weekend_publication_blocked"])


if __name__ == "__main__":
    unittest.main()
