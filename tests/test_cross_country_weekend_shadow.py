import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

try:
    from openpyxl import Workbook
except ModuleNotFoundError:
    Workbook = None

from pipelines.cross_country_weekend_shadow import (
    EXPECTED_HEADERS,
    SHADOW_SCHEMA,
    WeekendShadowError,
    validate_weekend_shadow,
)


def write_shadow(root: Path, *, publication: str = "disabled") -> tuple[Path, Path]:
    runtime = root / "runtime"
    predictions = root / "predictions"
    output = predictions / "20261127"
    runtime.mkdir()
    output.mkdir(parents=True)
    (runtime / "runtime_report.json").write_text(
        json.dumps(
            {
                "logical_date": "2026-11-27",
                "publication": {"status": "disabled"},
                "event_modes": {"M/101": "official_fis_startlist"},
            }
        ),
        encoding="utf-8",
    )
    (output / "shadow_manifest.json").write_text(
        json.dumps(
            {
                "schema_version": SHADOW_SCHEMA,
                "status": "candidate",
                "logical_date": "2026-11-27",
                "n_simulations": 250,
                "random_seed": 42,
                "publication": {"status": publication},
            }
        ),
        encoding="utf-8",
    )
    workbook = Workbook()
    sheet = workbook.active
    sheet.title = "Men Top 30"
    sheet.append(EXPECTED_HEADERS)
    sheet.append(("Athlete", 123, "Norway", "M", 100.5, 2, 25.0, 50.0))
    workbook.save(output / "weekend_top_30_predicted_points.xlsx")
    return runtime, predictions


@unittest.skipUnless(Workbook is not None, "openpyxl is not installed")
class CrossCountryWeekendShadowTests(unittest.TestCase):
    def test_validates_candidate_and_writes_checksummed_report(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime, predictions = write_shadow(root)
            report = validate_weekend_shadow(
                runtime_dir=runtime,
                prediction_root=predictions,
                logical_date=date(2026, 11, 27),
                n_simulations=250,
                random_seed=42,
                report_path=root / "reports/report.json",
            )
            self.assertEqual(report["status"], "passed")
            self.assertEqual(report["publication"], {"status": "disabled"})
            self.assertEqual(report["sheets"]["Men Top 30"]["rows"], 1)
            self.assertEqual(
                len(
                    report["outputs"]["weekend_top_30_predicted_points.xlsx"]["sha256"]
                ),
                64,
            )
            self.assertTrue((root / "reports/report.json").is_file())

    def test_rejects_publication_enabled_manifest(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime, predictions = write_shadow(root, publication="enabled")
            with self.assertRaisesRegex(WeekendShadowError, "manifest"):
                validate_weekend_shadow(
                    runtime_dir=runtime,
                    prediction_root=predictions,
                    logical_date=date(2026, 11, 27),
                    n_simulations=250,
                    random_seed=42,
                    report_path=root / "report.json",
                )

    def test_rejects_malformed_workbook(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime, predictions = write_shadow(root)
            workbook_path = (
                predictions / "20261127/weekend_top_30_predicted_points.xlsx"
            )
            workbook = Workbook()
            workbook.active.title = "Wrong Sheet"
            workbook.save(workbook_path)
            with self.assertRaisesRegex(WeekendShadowError, "sheets"):
                validate_weekend_shadow(
                    runtime_dir=runtime,
                    prediction_root=predictions,
                    logical_date=date(2026, 11, 27),
                    n_simulations=250,
                    random_seed=42,
                    report_path=root / "report.json",
                )


if __name__ == "__main__":
    unittest.main()
