import shutil
import subprocess
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SIMULATION = ROOT / "content/post/cross-country/drafts/weekly-picks-simulation.R"
WORKFLOW = ROOT / ".github/workflows/cross-country-weekend-shadow.yml"


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

    def test_history_free_athletes_can_use_validated_shadow_elo_features(self) -> None:
        self.assertIn("build_shadow_elo_newdata", self.source)
        self.assertIn("fixed_anchor_prediction_elo", self.source)
        self.assertIn("newcomer_prior", self.source)
        self.assertIn("features <- list(prev_points_weighted = 0)", self.source)
        self.assertIn("athlete_rating / field_max", self.source)
        self.assertIn("Using", self.source)
        self.assertIn("features for history-free athlete", self.source)

    def test_official_participation_is_applied_per_event(self) -> None:
        self.assertIn(
            'race_official_col <- paste0("Race", i, "_Official")', self.source
        )
        self.assertIn("Using official participation values from", self.source)
        self.assertIn("Invalid official participation values", self.source)
        self.assertNotIn("has_fis_startlist", self.source)

    def test_audited_quota_contract_precedes_legacy_fallback(self) -> None:
        contract = self.source.index("normalize_contract_quota_vector <- function")
        legacy = self.source.index('startlist$Quota <- 4')
        self.assertLess(contract, legacy)
        self.assertIn("Shared_Event_Quota", self.source)
        self.assertIn("Personal_Quota", self.source)
        self.assertIn("Max_Nation_Starters", self.source)
        self.assertIn("Partial FIS quota contract", self.source)
        self.assertIn("Legacy runtimes retain their historical fallback", self.source)

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

    def test_writes_full_pool_specialist_fantasy_input(self) -> None:
        self.assertIn(
            "safe_points <- apply(all_sims, 1, quantile, probs = 0.1", self.source
        )
        self.assertIn(
            "upside_points <- apply(all_sims, 1, quantile, probs = 0.9",
            self.source,
        )
        self.assertIn("Expected_Safe = safe_points * Race_Prob", self.source)
        self.assertIn("Expected_Upside = upside_points * Race_Prob", self.source)
        self.assertIn('Race_Discipline = if_else(race$distance == "Sprint"', self.source)
        self.assertIn("`Sprint Points` = round(Sprint_Points, 2)", self.source)
        self.assertIn("`Distance Points` = round(Distance_Points, 2)", self.source)
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

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_shadow_elo_feature_builder_executes_in_r(self) -> None:
        expression = f"""
        expressions <- parse(file={str(SIMULATION)!r})
        matches <- Filter(function(expr) {{
          is.call(expr) && as.character(expr[[1]]) == '<-' &&
            as.character(expr[[2]]) == 'build_shadow_elo_newdata'
        }}, expressions)
        stopifnot(length(matches) == 1)
        eval(matches[[1]], envir=.GlobalEnv)
        rating_names <- c(
          'Elo', 'Distance_Elo', 'Distance_C_Elo', 'Distance_F_Elo',
          'Sprint_Elo', 'Sprint_C_Elo', 'Sprint_F_Elo',
          'Classic_Elo', 'Freestyle_Elo'
        )
        athlete <- data.frame(Shadow_Rating_Source='fixed_anchor_prediction_elo')
        field <- data.frame(Shadow_Rating_Source=c(
          'accepted_official_elo', 'fixed_anchor_prediction_elo'
        ))
        for (name in rating_names) {{
          athlete[[name]] <- 1400
          field[[name]] <- c(1600, 1400)
        }}
        result <- build_shadow_elo_newdata(athlete, field)
        stopifnot(result$prev_points_weighted == 0)
        stopifnot(abs(result$Pelo_pct - 0.875) < 1e-12)
        athlete$Shadow_Rating_Source <- 'accepted_official_elo'
        stopifnot(is.null(build_shadow_elo_newdata(athlete, field)))
        """
        result = subprocess.run(
            ["Rscript", "-e", expression],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)


class CrossCountryOfficialQuotaShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_manual_quota_mode_fetches_and_supplies_audited_contract(self) -> None:
        self.assertIn("official_quota_shadow:", self.source)
        self.assertIn("default: false", self.source)
        self.assertIn("cross_country_announced_rosters.py", self.source)
        self.assertIn("cross_country_world_cup_quotas.py", self.source)
        self.assertIn("cross_country_world_cup_quotas_2027.json", self.source)
        self.assertIn(
            "--quota-contract cross_country_world_cup_quotas_2027.json",
            self.source,
        )
        self.assertNotIn("OFFICIAL_QUOTA_SHADOW", self.source)

    def test_latest_automatic_roster_is_restored_before_bundle_build(self) -> None:
        restore = self.source.index("name: Restore latest automatic FantasyXC roster")
        build = self.source.index("name: Build isolated weekend input bundle")
        self.assertLess(restore, build)
        self.assertIn("fantasyxc-roster-v2-${{ env.LOGICAL_DATE }}-", self.source)
        self.assertIn('args+=(--announced-rosters "$roster")', self.source)
        self.assertIn("fis-prediction-ledger.csv", self.source)
        self.assertIn('--prediction-elo-root "$RATING_ROOT"', self.source)

    def test_quota_shadow_cannot_install_or_publish(self) -> None:
        install = self.source.split("name: Install validated weekend candidate", 1)[1]
        install = install.split("- name: Publish validated weekend predictions", 1)[0]
        publish = self.source.split("name: Publish validated weekend predictions", 1)[1]
        publish = publish.split("- name: Retain complete shadow evidence", 1)[0]
        self.assertIn("inputs.official_quota_shadow != true", install)
        self.assertIn("inputs.official_quota_shadow != true", publish)
        self.assertIn("if: always()", self.source)

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_official_quota_math_executes_in_r(self) -> None:
        expression = f"""
        expressions <- parse(file={str(SIMULATION)!r})
        matches <- Filter(function(expr) {{
          is.call(expr) && as.character(expr[[1]]) == '<-' &&
            as.character(expr[[2]]) == 'normalize_contract_quota_vector'
        }}, expressions)
        stopifnot(length(matches) == 1)
        eval(matches[[1]], envir=.GlobalEnv)

        norway <- normalize_contract_quota_vector(
          c(rep(0.5, 6), 1, 1), 6, c(rep(0, 6), 1, 1), 10
        )
        stopifnot(abs(sum(norway[1:6]) - 6) < 1e-12)
        stopifnot(abs(sum(norway) - 8) < 1e-12)

        non_transferable <- normalize_contract_quota_vector(
          c(rep(0.5, 6), 1, 0), 6, c(rep(0, 6), 1, 1), 10
        )
        stopifnot(abs(sum(non_transferable[1:6]) - 6) < 1e-12)
        stopifnot(abs(sum(non_transferable) - 7) < 1e-12)

        finland_host <- normalize_contract_quota_vector(
          c(rep(0.5, 10), 1), 10, c(rep(0, 10), 1), 10
        )
        stopifnot(abs(sum(finland_host[1:10]) - 9) < 1e-12)
        stopifnot(abs(sum(finland_host) - 10) < 1e-12)

        france_women <- normalize_contract_quota_vector(
          rep(0.5, 5), 5, rep(0, 5), 10
        )
        stopifnot(abs(sum(france_women) - 5) < 1e-12)

        team_weekend <- normalize_contract_quota_vector(
          rep(0.5, 4), 4, rep(0, 4), 10
        )
        stopifnot(abs(sum(team_weekend) - 4) < 1e-12)

        skewed <- normalize_contract_quota_vector(
          c(0.95, rep(0.05, 5)), 5, rep(0, 6), 10
        )
        stopifnot(all(skewed <= 1))
        stopifnot(abs(sum(skewed) - 5) < 1e-12)
        """
        result = subprocess.run(
            ["Rscript", "-e", expression],
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
