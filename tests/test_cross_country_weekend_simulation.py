import shutil
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SIMULATION = ROOT / "content/post/cross-country/drafts/weekly-picks-simulation.R"


class CrossCountryWeekendSimulationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = SIMULATION.read_text(encoding="utf-8")

    def test_uses_only_explicit_shadow_runtime_paths(self) -> None:
        for variable in (
            "NORDIC_BLOG_ROOT",
            "NORDIC_WEEKEND_RUNTIME_ROOT",
            "NORDIC_SKI_DATA_ROOT",
            "NORDIC_PREDICTION_OUTPUT_ROOT",
            "NORDIC_LOG_ROOT",
            "NORDIC_LOGICAL_DATE",
        ):
            self.assertIn(variable, self.source)
        self.assertNotIn('"~/', self.source)
        self.assertNotIn("Sys.time()", self.source)
        self.assertIn('runtime_path("weekends.csv")', self.source)
        self.assertIn('runtime_path("startlist_weekend_men.csv")', self.source)
        self.assertIn('ski_data_path("men_chrono_elevation.csv")', self.source)

    def test_simulation_is_deterministic_and_publication_disabled(self) -> None:
        self.assertIn("NORDIC_N_SIMULATIONS", self.source)
        self.assertIn("NORDIC_RANDOM_SEED", self.source)
        self.assertIn("set.seed(RANDOM_SEED)", self.source)
        self.assertIn('"publication": {"status": "disabled"}', self.source)
        self.assertIn("nordicnumbers.cross-country-weekend-shadow/v1", self.source)

    def test_official_participation_is_applied_per_event(self) -> None:
        self.assertIn(
            'race_official_col <- paste0("Race", i, "_Official")', self.source
        )
        self.assertIn("Using official participation values from", self.source)
        self.assertIn("Invalid official participation values", self.source)
        self.assertNotIn("has_fis_startlist", self.source)

    def test_normal_weekend_does_not_execute_fantasy_optimization(self) -> None:
        self.assertNotIn("fantasy_team <- optimize_weekly_team_simulation", self.source)
        self.assertNotIn("fantasy_team.xlsx", self.source)
        self.assertNotIn("library(ompr)", self.source)
        self.assertNotIn("library(ROI.plugin.glpk)", self.source)
        self.assertIn("Fantasy optimization is a separate product", self.source)

    def test_writes_complete_multisheet_workbook_with_writexl(self) -> None:
        self.assertIn("library(writexl)", self.source)
        self.assertIn(
            "write_xlsx(weekly_points_sheets, weekly_points_file)", self.source
        )
        self.assertNotIn("library(openxlsx)", self.source)
        self.assertNotIn("write.xlsx(weekly_points_sheets", self.source)
        self.assertEqual(self.source.count("ID = athlete_id"), 2)

    def test_writes_full_pool_three_objective_fantasy_input(self) -> None:
        self.assertIn(
            "safe_points <- apply(all_sims, 1, quantile, probs = 0.1", self.source
        )
        self.assertIn(
            "upside_points <- apply(all_sims, 1, quantile, probs = 0.9",
            self.source,
        )
        self.assertIn("Expected_Safe = safe_points * Race_Prob", self.source)
        self.assertIn("Expected_Upside = upside_points * Race_Prob", self.source)
        self.assertIn('fantasy_input_sheets[["Men Full Pool"]]', self.source)
        self.assertIn('fantasy_input_sheets[["Ladies Full Pool"]]', self.source)
        self.assertIn('"weekend_fantasy_inputs.xlsx"', self.source)
        self.assertIn("`Logical Date` = LOGICAL_DATE_TEXT", self.source)
        self.assertIn("`Random Seed` = RANDOM_SEED", self.source)
        self.assertIn("`Simulation Count` = N_SIMULATIONS", self.source)

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_r_source_parses(self) -> None:
        result = subprocess.run(
            ["Rscript", "-e", f"parse(file={str(SIMULATION)!r})"],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
