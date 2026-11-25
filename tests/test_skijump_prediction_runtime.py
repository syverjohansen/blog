from __future__ import annotations

import csv
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
SIMULATION = ROOT / "content/post/skijump/drafts/race-picks-simulation.R"
INVENTORY = ROOT / "pipelines/skijump_prediction_inventory.json"
RSCRIPT = shutil.which("Rscript")
PACKAGES = ("dplyr", "tidyr", "openxlsx", "mgcv", "leaps", "logger", "lubridate")


def _runtime_available() -> bool:
    if not RSCRIPT:
        return False
    expression = (
        "packages <- c(" + ",".join(f'\"{package}\"' for package in PACKAGES) + ");"
        "quit(status=as.integer(any(!vapply(packages, requireNamespace, "
        "logical(1), quietly=TRUE))))"
    )
    return subprocess.run(
        (RSCRIPT, "-e", expression),
        capture_output=True,
        text=True,
        check=False,
    ).returncode == 0


class SkiJumpingPredictionRuntimeTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SIMULATION.read_text(encoding="utf-8")
        cls.runtime_available = _runtime_available()

    def test_runtime_contract_parameterizes_all_mutable_boundaries(self) -> None:
        for variable in (
            "NORDIC_INPUT_ROOT",
            "NORDIC_TEAM_INPUT_ROOT",
            "NORDIC_OUTPUT_ROOT",
            "NORDIC_BLOG_ROOT",
            "NORDIC_LOG_ROOT",
            "NORDIC_LOGICAL_DATE",
            "NORDIC_RUN_ID",
            "NORDIC_RANDOM_SEED",
            "NORDIC_N_SIMULATIONS",
        ):
            self.assertIn(variable, self.source)
        self.assertIn("set.seed(NORDIC_RANDOM_SEED)", self.source)
        self.assertIn("output_dir <- file.path(NORDIC_OUTPUT_ROOT, utc_date)", self.source)
        self.assertNotIn('base_path <- "~/ski', self.source)
        self.assertNotIn('team_base_path <- "~/ski', self.source)
        self.assertNotIn('output_dir <- paste0("~/blog', self.source)

    @unittest.skipUnless(RSCRIPT, "Rscript is not installed")
    def test_r_source_parses(self) -> None:
        result = subprocess.run(
            (RSCRIPT, "-e", f"parse(file={str(SIMULATION)!r})"),
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(0, result.returncode, result.stderr)

    def test_publication_disabled_inventory_records_runtime_contract(self) -> None:
        import json

        inventory = json.loads(INVENTORY.read_text(encoding="utf-8"))
        contract = inventory["model_runtime_contract"]
        self.assertEqual("NORDIC_INPUT_ROOT", contract["input_root"])
        self.assertEqual("NORDIC_TEAM_INPUT_ROOT", contract["team_input_root"])
        self.assertEqual("NORDIC_OUTPUT_ROOT", contract["output_root"])
        self.assertEqual("NORDIC_RANDOM_SEED", contract["random_seed"])
        self.assertEqual("NORDIC_N_SIMULATIONS", contract["simulation_count"])

    def test_isolated_no_race_run_uses_logical_date_and_explicit_roots(self) -> None:
        if not self.runtime_available:
            self.skipTest("Rscript prediction dependencies are not installed")
        with tempfile.TemporaryDirectory() as directory:
            workspace = Path(directory)
            input_root = workspace / "individual"
            team_root = workspace / "team"
            output_root = workspace / "output"
            log_root = workspace / "logs"
            input_root.mkdir()
            team_root.mkdir()
            with (input_root / "races.csv").open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(handle, fieldnames=("Date",))
                writer.writeheader()
                writer.writerow({"Date": "01/01/2000"})
            environment = {
                **os.environ,
                "NORDIC_INPUT_ROOT": str(input_root),
                "NORDIC_TEAM_INPUT_ROOT": str(team_root),
                "NORDIC_OUTPUT_ROOT": str(output_root),
                "NORDIC_BLOG_ROOT": str(ROOT),
                "NORDIC_LOG_ROOT": str(log_root),
                "NORDIC_LOGICAL_DATE": "2026-02-10",
                "NORDIC_RUN_ID": "runtime-test",
                "NORDIC_RANDOM_SEED": "42",
                "NORDIC_N_SIMULATIONS": "5",
                "NORDIC_LOAD_ENV": "false",
                "NORDIC_ENHANCED_LOGGING": "false",
            }
            result = subprocess.run(
                (RSCRIPT, str(SIMULATION)),
                cwd=workspace,
                env=environment,
                capture_output=True,
                text=True,
                check=False,
            )

            self.assertEqual(0, result.returncode, result.stderr)
            self.assertIn("No races found for today", (log_root / "simulation.log").read_text())
            self.assertFalse(output_root.exists())

    def test_invalid_numeric_runtime_parameters_fail_closed(self) -> None:
        if not self.runtime_available:
            self.skipTest("Rscript prediction dependencies are not installed")
        cases = (
            ("NORDIC_RANDOM_SEED", "not-an-integer", "must be an integer"),
            ("NORDIC_N_SIMULATIONS", "0", "must be a positive integer"),
        )
        for variable, value, message in cases:
            with self.subTest(variable=variable):
                result = subprocess.run(
                    (RSCRIPT, str(SIMULATION)),
                    env={
                        **os.environ,
                        "NORDIC_BLOG_ROOT": str(ROOT),
                        "NORDIC_LOAD_ENV": "false",
                        variable: value,
                    },
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertNotEqual(0, result.returncode)
                self.assertIn(message, result.stderr)


if __name__ == "__main__":
    unittest.main()
