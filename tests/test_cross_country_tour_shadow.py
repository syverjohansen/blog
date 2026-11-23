import csv
import json
import os
import shutil
import subprocess
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_tour_shadow import TourShadowError, validate_tour_shadow

ROOT = Path(__file__).resolve().parents[1]
SIMULATION = ROOT / "content/post/cross-country/drafts/tds-picks-simulation.R"


def write_csv(path: Path, fields: list[str], rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def write_runtime(root: Path) -> Path:
    runtime = root / "runtime"
    runtime.mkdir()
    for name, rows in (
        (
            "startlist_tds_men.csv",
            [
                {"ID": "1", "Skier": "Men One", "Nation": "Norway", "Price": "0"},
                {"ID": "2", "Skier": "Men Two", "Nation": "Sweden", "Price": "0"},
            ],
        ),
        (
            "startlist_tds_ladies.csv",
            [
                {"ID": "11", "Skier": "Women One", "Nation": "Norway", "Price": "0"},
                {"ID": "12", "Skier": "Women Two", "Nation": "Sweden", "Price": "0"},
            ],
        ),
    ):
        write_csv(runtime / name, ["ID", "Skier", "Nation", "Price"], rows)
    (runtime / "report.json").write_text(
        json.dumps(
            {
                "schema_version": "nordicnumbers.cross-country-tour-inputs/v1",
                "status": "candidate",
                "logical_date": "2027-01-01",
                "publication": {"status": "disabled"},
            }
        ),
        encoding="utf-8",
    )
    return runtime


def write_outputs(root: Path, *, publication: str = "disabled") -> Path:
    from openpyxl import Workbook

    output = root / "predictions" / "20270101"
    output.mkdir(parents=True)
    (output / "shadow_manifest.json").write_text(
        json.dumps(
            {
                "schema_version": "nordicnumbers.cross-country-tour-shadow/v1",
                "status": "candidate",
                "logical_date": "2027-01-01",
                "n_simulations": 100,
                "random_seed": 42,
                "publication": {"status": publication},
            }
        ),
        encoding="utf-8",
    )
    identities = {
        "men": [("Men One", 1, "Norway"), ("Men Two", 2, "Sweden")],
        "ladies": [("Women One", 11, "Norway"), ("Women Two", 12, "Sweden")],
    }
    for gender, rows in identities.items():
        wb = Workbook()
        ws = wb.active
        ws.append(list(("Skier", "ID", "Nation", "Points")))
        for index, identity in enumerate(rows):
            ws.append([*identity, 200 - index * 20])
        wb.save(output / f"{gender}.xlsx")

        wb = Workbook()
        ws = wb.active
        ws.title = "Men Tour de Ski" if gender == "men" else "Ladies Tour de Ski"
        ws.append(list(("Skier", "ID", "Nation", "Win", "Podium", "Top-5", "Top-10", "Top-30")))
        for index, identity in enumerate(rows):
            ws.append([*identity, 20 - index * 5, 40, 60, 80, 100])
        wb.save(output / f"{gender}_position_probabilities.xlsx")
    return root / "predictions"


def write_history(path: Path, id_offset: int) -> None:
    rows: list[dict[str, object]] = []
    for season in range(2023, 2027):
        for race in range(1, 13):
            for athlete in range(1, 7):
                place = ((athlete + race) % 6) + 1
                rows.append(
                    {
                        "Date": f"{season - 1}-12-{race + 1:02d}",
                        "Season": season,
                        "Race": race,
                        "ID": id_offset + athlete,
                        "Skier": (
                            f"Men {athlete}" if id_offset == 0 else f"Women {athlete}"
                        ),
                        "Nation": "Norway" if athlete % 2 else "Sweden",
                        "Distance": "10",
                        "Event": "Tour de Ski" if race <= 4 else "World Cup",
                        "City": "Tour De Ski" if race <= 4 else "Davos",
                        "Place": place,
                        "Pelo": 1400 + athlete * 41 + race * (athlete + 2) + season % 7,
                        "Distance_F_Pelo": 1350
                        + athlete * 37
                        + race * (athlete + 1),
                    }
                )
    write_csv(path, list(rows[0]), rows)


def workbook_signature(path: Path) -> tuple[tuple[str, tuple[tuple[object, ...], ...]], ...]:
    from openpyxl import load_workbook

    workbook = load_workbook(path, read_only=True, data_only=True)
    signature = tuple(
        (name, tuple(workbook[name].iter_rows(values_only=True)))
        for name in workbook.sheetnames
    )
    workbook.close()
    return signature


class CrossCountryTourShadowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SIMULATION.read_text(encoding="utf-8")

    def test_simulation_uses_only_explicit_shadow_paths_and_seed(self) -> None:
        for variable in (
            "NORDIC_TOUR_RUNTIME_ROOT",
            "NORDIC_SKI_DATA_ROOT",
            "NORDIC_PREDICTION_OUTPUT_ROOT",
            "NORDIC_LOG_ROOT",
            "NORDIC_LOGICAL_DATE",
            "NORDIC_N_SIMULATIONS",
            "NORDIC_RANDOM_SEED",
        ):
            self.assertIn(variable, self.source)
        self.assertNotIn('"~/', self.source)
        self.assertNotIn("Sys.Date()", self.source)
        self.assertNotIn("Sys.time()", self.source)
        self.assertIn("set.seed(RANDOM_SEED)", self.source)
        self.assertIn('runtime_path("startlist_tds_men.csv")', self.source)
        self.assertIn('ski_data_path("men_chrono_elevation.csv")', self.source)
        self.assertIn('"publication": {"status": "disabled"}', self.source)

    def test_validates_complete_publication_disabled_output(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime = write_runtime(root)
            predictions = write_outputs(root)
            report = validate_tour_shadow(
                runtime_dir=runtime,
                prediction_root=predictions,
                logical_date=date(2027, 1, 1),
                n_simulations=100,
                random_seed=42,
                report_path=root / "validation.json",
            )
            self.assertEqual("passed", report["status"])
            self.assertEqual({"rows": 2}, report["sexes"]["M"])
            self.assertEqual({"status": "disabled"}, report["publication"])

    def test_rejects_enabled_publication(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime = write_runtime(root)
            predictions = write_outputs(root, publication="enabled")
            with self.assertRaisesRegex(TourShadowError, "manifest"):
                validate_tour_shadow(
                    runtime_dir=runtime,
                    prediction_root=predictions,
                    logical_date=date(2027, 1, 1),
                    n_simulations=100,
                    random_seed=42,
                    report_path=root / "validation.json",
                )

    def test_rejects_output_identity_not_in_official_startlist(self) -> None:
        from openpyxl import load_workbook

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime = write_runtime(root)
            predictions = write_outputs(root)
            path = predictions / "20270101" / "men.xlsx"
            workbook = load_workbook(path)
            workbook.active["B2"] = 999
            workbook.save(path)
            workbook.close()
            with self.assertRaisesRegex(TourShadowError, "identities"):
                validate_tour_shadow(
                    runtime_dir=runtime,
                    prediction_root=predictions,
                    logical_date=date(2027, 1, 1),
                    n_simulations=100,
                    random_seed=42,
                    report_path=root / "validation.json",
                )

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_same_seed_produces_identical_shadow_values(self) -> None:
        packages = subprocess.run(
            [
                "Rscript",
                "-e",
                (
                    'stopifnot(all(vapply(c("dplyr","tidyr","writexl","mgcv",'
                    '"leaps","logger","purrr","lubridate"), requireNamespace, '
                    'logical(1), quietly=TRUE)))'
                ),
            ],
            capture_output=True,
            text=True,
            check=False,
        )
        if packages.returncode:
            self.skipTest("Tour simulation R packages are not installed")

        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            runtime = write_runtime(root)
            ski_data = root / "ski-data"
            write_history(ski_data / "men_chrono_elevation.csv", 0)
            write_history(ski_data / "ladies_chrono_elevation.csv", 10)
            signatures = []
            for attempt in ("one", "two"):
                prediction_root = root / f"predictions-{attempt}"
                env = {
                    **os.environ,
                    "NORDIC_TOUR_RUNTIME_ROOT": str(runtime),
                    "NORDIC_SKI_DATA_ROOT": str(ski_data),
                    "NORDIC_PREDICTION_OUTPUT_ROOT": str(prediction_root),
                    "NORDIC_LOG_ROOT": str(root / f"logs-{attempt}"),
                    "NORDIC_LOGICAL_DATE": "2027-01-01",
                    "NORDIC_N_SIMULATIONS": "100",
                    "NORDIC_RANDOM_SEED": "42",
                }
                result = subprocess.run(
                    ["Rscript", str(SIMULATION)],
                    env=env,
                    capture_output=True,
                    text=True,
                    check=False,
                    timeout=60,
                )
                self.assertEqual(0, result.returncode, result.stderr + result.stdout)
                output = prediction_root / "20270101"
                signatures.append(
                    tuple(
                        workbook_signature(output / name)
                        for name in (
                            "men.xlsx",
                            "ladies.xlsx",
                            "men_position_probabilities.xlsx",
                            "ladies_position_probabilities.xlsx",
                        )
                    )
                )
                report = validate_tour_shadow(
                    runtime_dir=runtime,
                    prediction_root=prediction_root,
                    logical_date=date(2027, 1, 1),
                    n_simulations=100,
                    random_seed=42,
                    report_path=root / f"validation-{attempt}.json",
                )
                self.assertEqual("passed", report["status"])
            self.assertEqual(signatures[0], signatures[1])

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_r_source_parses(self) -> None:
        result = subprocess.run(
            ["Rscript", "-e", f"parse(file={str(SIMULATION)!r})"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, result.returncode, result.stderr)


if __name__ == "__main__":
    unittest.main()
