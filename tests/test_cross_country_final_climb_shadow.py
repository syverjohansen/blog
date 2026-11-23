import csv
import json
import shutil
import subprocess
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_final_climb_shadow import (
    FinalClimbShadowError,
    validate_final_climb_shadow,
)

ROOT = Path(__file__).resolve().parents[1]
SIMULATION = ROOT / "content/post/cross-country/drafts/final_climb-simulation.R"


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=["ID", "Skier", "Nation", "Price"])
        writer.writeheader()
        writer.writerows(rows)


def write_runtime(root: Path) -> Path:
    runtime = root / "runtime"
    runtime.mkdir()
    write_csv(
        runtime / "startlist_final_climb_men.csv",
        [
            {"ID": "1", "Skier": "Men One", "Nation": "Norway", "Price": "0"},
            {"ID": "2", "Skier": "Men Two", "Nation": "Sweden", "Price": "0"},
        ],
    )
    write_csv(
        runtime / "startlist_final_climb_ladies.csv",
        [
            {"ID": "11", "Skier": "Women One", "Nation": "Norway", "Price": "0"},
            {"ID": "12", "Skier": "Women Two", "Nation": "Sweden", "Price": "0"},
        ],
    )
    (runtime / "report.json").write_text(
        json.dumps(
            {
                "schema_version": "nordicnumbers.cross-country-final-climb-inputs/v1",
                "status": "candidate",
                "logical_date": "2027-01-09",
                "plan": {"final_climb_date": "2027-01-10"},
                "publication": {"status": "disabled"},
            }
        ),
        encoding="utf-8",
    )
    return runtime


def write_outputs(root: Path, *, publication: str = "disabled") -> Path:
    from openpyxl import Workbook

    output = root / "predictions" / "20270110"
    output.mkdir(parents=True)
    (output / "shadow_manifest.json").write_text(
        json.dumps(
            {
                "schema_version": "nordicnumbers.cross-country-final-climb-shadow/v1",
                "status": "candidate",
                "logical_date": "2027-01-10",
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
        workbook = Workbook()
        sheet = workbook.active
        sheet.append(["Skier", "ID", "Nation", "Points"])
        for index, identity in enumerate(rows):
            sheet.append([*identity, 45 - index * 5])
        workbook.save(output / f"{gender}.xlsx")

        workbook = Workbook()
        sheet = workbook.active
        sheet.title = "Men Race 1" if gender == "men" else "Ladies Race 1"
        sheet.append(
            ["Skier", "ID", "Nation", "Win", "Podium", "Top-5", "Top-10", "Top-30"]
        )
        for index, identity in enumerate(rows):
            sheet.append([*identity, 20 - index * 5, 40, 60, 80, 100])
        workbook.save(output / f"{gender}_position_probabilities.xlsx")
    return root / "predictions"


class CrossCountryFinalClimbShadowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SIMULATION.read_text(encoding="utf-8")

    def test_simulation_uses_only_explicit_shadow_paths_and_seed(self) -> None:
        for variable in (
            "NORDIC_FINAL_CLIMB_RUNTIME_ROOT",
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
        self.assertIn('runtime_path("startlist_final_climb_men.csv")', self.source)
        self.assertIn('ski_data_path("men_chrono_elevation.csv")', self.source)
        self.assertIn('"publication": {"status": "disabled"}', self.source)

    def test_validates_complete_publication_disabled_output(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            report = validate_final_climb_shadow(
                runtime_dir=write_runtime(root),
                prediction_root=write_outputs(root),
                logical_date=date(2027, 1, 10),
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
            with self.assertRaisesRegex(FinalClimbShadowError, "manifest"):
                validate_final_climb_shadow(
                    runtime_dir=write_runtime(root),
                    prediction_root=write_outputs(root, publication="enabled"),
                    logical_date=date(2027, 1, 10),
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
            path = predictions / "20270110" / "men.xlsx"
            workbook = load_workbook(path)
            workbook.active["B2"] = 999
            workbook.save(path)
            workbook.close()
            with self.assertRaisesRegex(FinalClimbShadowError, "identities"):
                validate_final_climb_shadow(
                    runtime_dir=runtime,
                    prediction_root=predictions,
                    logical_date=date(2027, 1, 10),
                    n_simulations=100,
                    random_seed=42,
                    report_path=root / "validation.json",
                )

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
