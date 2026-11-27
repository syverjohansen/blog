import hashlib
import json
import tempfile
import unittest
from datetime import UTC, date, datetime
from pathlib import Path
from urllib.error import URLError

try:
    from openpyxl import Workbook
except ModuleNotFoundError:
    Workbook = None

from pipelines.cross_country_fantasy_prices import (
    FANTASY_EXPECTED_HEADERS,
    PRICE_SOURCE,
    capture_and_enrich_prices,
)


class FakeResponse:
    status = 200

    def __init__(self, payload: bytes):
        self.payload = payload

    def __enter__(self):
        return self

    def __exit__(self, *args):
        return None

    def read(self) -> bytes:
        return self.payload


def write_workbook(path: Path, *, missing_fis: bool = False) -> list[dict[str, object]]:
    workbook = Workbook()
    prices: list[dict[str, object]] = []
    for sex, title, offset in (("M", "Men Full Pool", 0), ("L", "Ladies Full Pool", 8)):
        sheet = workbook.active if offset == 0 else workbook.create_sheet()
        sheet.title = title
        sheet.append(FANTASY_EXPECTED_HEADERS)
        for local_index in range(8):
            index = offset + local_index
            fis_code = 3_420_001 + index
            sheet.append(
                (
                    str(100 + index),
                    None if missing_fis and index == 15 else str(fis_code),
                    f"Prediction Name {index}",
                    "Norway",
                    sex,
                    100 - index,
                    80 - index,
                    120 - index,
                    40 - index,
                    60 - index,
                    2,
                    1,
                    1,
                    "2026-11-27",
                    42,
                    250,
                )
            )
            prices.append(
                {
                    "athlete_id": fis_code,
                    "name": f"API NAME {index}",
                    "price": 5_000,
                    "country": "NOR",
                    "gender": "m" if sex == "M" else "f",
                    "active": True,
                    "is_team": False,
                }
            )
    workbook.save(path)
    return prices


@unittest.skipUnless(Workbook is not None, "openpyxl is not installed")
class CrossCountryFantasyPriceTests(unittest.TestCase):
    def run_capture(
        self,
        root: Path,
        *,
        payload: bytes | None = None,
        missing_fis: bool = False,
        opener=None,
    ):
        workbook = root / "weekend_fantasy_inputs.xlsx"
        prices = write_workbook(workbook, missing_fis=missing_fis)
        payload = payload if payload is not None else json.dumps(prices).encode()
        if opener is None:
            opener = lambda *_args, **_kwargs: FakeResponse(payload)
        report = capture_and_enrich_prices(
            workbook_path=workbook,
            logical_date=date(2026, 11, 27),
            random_seed=42,
            n_simulations=250,
            output_dir=root / "fantasy",
            report_path=root / "reports/fantasy.json",
            opener=opener,
            sleep=lambda _seconds: None,
            now=lambda: datetime(2026, 11, 27, 12, tzinfo=UTC),
        )
        return report, prices

    def test_captures_and_enriches_by_exact_fis_code(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            report, prices = self.run_capture(root)
            self.assertEqual("candidate", report["status"])
            self.assertFalse(report["weekend_publication_blocked"])
            raw = root / "fantasy/fantasy_xc_prices.json"
            priced = root / "fantasy/fantasy_priced_inputs.json"
            self.assertTrue(raw.is_file())
            document = json.loads(priced.read_text())
            self.assertEqual(16, len(document["rows"]))
            self.assertEqual(prices[0]["price"], document["rows"][0]["price"])
            self.assertEqual(prices[0]["name"], document["rows"][0]["fantasy_xc_name"])
            self.assertEqual(
                hashlib.sha256(raw.read_bytes()).hexdigest(),
                report["source"]["sha256"],
            )
            self.assertEqual(PRICE_SOURCE, report["source"]["url"])

    def test_api_failure_is_retained_as_non_blocking_unavailable(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            calls = []

            def unavailable(*_args, **_kwargs):
                calls.append(True)
                raise URLError("offline")

            root = Path(directory)
            report, _ = self.run_capture(root, opener=unavailable)
            self.assertEqual(3, len(calls))
            self.assertEqual("unavailable", report["status"])
            self.assertFalse(report["weekend_publication_blocked"])
            self.assertTrue((root / "reports/fantasy.json").is_file())
            self.assertFalse((root / "fantasy/fantasy_xc_prices.json").exists())

    def test_missing_exact_identity_blocks_only_fantasy(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            report, _ = self.run_capture(root, missing_fis=True)
            self.assertEqual("blocked", report["status"])
            self.assertFalse(report["weekend_publication_blocked"])
            self.assertEqual(15, report["identity_resolution"]["eligible_rows"])
            self.assertFalse((root / "fantasy/fantasy_priced_inputs.json").exists())

    def test_blank_fis_code_uses_unique_exact_active_name_tokens(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook = root / "weekend_fantasy_inputs.xlsx"
            prices = write_workbook(workbook, missing_fis=True)
            prices[-1]["name"] = "15 NAME Prediction"
            report = capture_and_enrich_prices(
                workbook_path=workbook,
                logical_date=date(2026, 11, 27),
                random_seed=42,
                n_simulations=250,
                output_dir=root / "fantasy",
                report_path=root / "report.json",
                opener=lambda *_args, **_kwargs: FakeResponse(
                    json.dumps(prices).encode()
                ),
                sleep=lambda _seconds: None,
            )
            self.assertEqual("candidate", report["status"])
            self.assertEqual(1, report["identity_resolution"]["exact_name_bridges"])
            document = json.loads(
                (root / "fantasy/fantasy_priced_inputs.json").read_text()
            )
            bridged = document["rows"][-1]
            self.assertEqual(str(prices[-1]["athlete_id"]), bridged["fis_code"])
            self.assertEqual(
                "exact_active_name_tokens", bridged["identity_match_method"]
            )

    def test_same_name_with_wrong_fis_code_does_not_match(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook = root / "weekend_fantasy_inputs.xlsx"
            prices = write_workbook(workbook)
            prices[-1]["athlete_id"] = 9_999_999
            prices[-1]["name"] = "Prediction Name 15"
            report = capture_and_enrich_prices(
                workbook_path=workbook,
                logical_date=date(2026, 11, 27),
                random_seed=42,
                n_simulations=250,
                output_dir=root / "fantasy",
                report_path=root / "report.json",
                opener=lambda *_args, **_kwargs: FakeResponse(
                    json.dumps(prices).encode()
                ),
                sleep=lambda _seconds: None,
            )
            self.assertEqual("blocked", report["status"])
            self.assertEqual(15, report["identity_resolution"]["eligible_rows"])
            self.assertIn("8 men / 8 ladies", report["reason"])

    def test_malformed_payload_is_captured_and_blocked(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            report, _ = self.run_capture(root, payload=b"not-json")
            self.assertEqual("blocked", report["status"])
            self.assertTrue((root / "fantasy/fantasy_xc_prices.json").is_file())
            self.assertFalse((root / "fantasy/fantasy_priced_inputs.json").exists())


if __name__ == "__main__":
    unittest.main()
