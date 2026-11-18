import shutil
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = (
    ROOT / "content/post/cross-country/drafts/race-picks-relay.R",
    ROOT / "content/post/cross-country/drafts/race-picks-team-sprint.R",
)
CONTRACT = ROOT / "pipelines/cross_country_team_prediction_contract.R"


class CrossCountryTeamPredictionTests(unittest.TestCase):
    def test_scripts_use_configurable_artifact_roots_and_contract(self) -> None:
        for script in SCRIPTS:
            source = script.read_text(encoding="utf-8")
            for variable in (
                "NORDIC_SKI_DATA_ROOT",
                "NORDIC_TEAM_DATA_ROOT",
                "NORDIC_BLOG_ROOT",
                "NORDIC_PREDICTION_OUTPUT_ROOT",
                "NORDIC_LOG_ROOT",
            ):
                self.assertIn(variable, source, script)
            self.assertIn("source(contract_path)", source, script)
            self.assertIn("validate_team_prediction_tables(", source, script)
            self.assertNotIn(
                'paste0("~/blog/daehl-e/content/post/cross-country/drafts/race-picks/"',
                source,
                script,
            )
            self.assertNotIn(
                'sprintf("~/ski/elo/python/ski/polars/relay/excel365/startlist_',
                source,
                script,
            )

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_r_sources_parse(self) -> None:
        for source in (*SCRIPTS, CONTRACT):
            result = subprocess.run(
                ["Rscript", "-e", f"parse(file={str(source)!r})"],
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(result.returncode, 0, result.stderr)

    @unittest.skipUnless(shutil.which("Rscript"), "Rscript is not installed")
    def test_contract_accepts_valid_output_and_rejects_bad_probabilities(self) -> None:
        program = textwrap.dedent(
            f"""
            source({str(CONTRACT)!r})
            points <- data.frame(
              Team = c("Norway I", "Sweden I"),
              `Leg 1` = c("A", "C"),
              `Leg 2` = c("B", "D"),
              `Expected Points` = c(180, 150),
              check.names = FALSE
            )
            probabilities <- data.frame(
              Team = c("Norway I", "Sweden I"),
              `Leg 1` = c("A", "C"),
              `Leg 2` = c("B", "D"),
              Win = c(0.4, 0.3),
              Podium = c(0.7, 0.6),
              `Top 5` = c(0.9, 0.8),
              `Top 10` = c(1, 1),
              check.names = FALSE
            )
            validate_team_prediction_tables(points, probabilities, 2)
            probabilities$Win[1] <- 1.1
            failed <- inherits(
              try(validate_team_prediction_tables(points, probabilities, 2), silent = TRUE),
              "try-error"
            )
            if (!failed) stop("invalid probabilities passed validation")
            """
        )
        with tempfile.TemporaryDirectory() as directory:
            test_file = Path(directory) / "contract-test.R"
            test_file.write_text(program, encoding="utf-8")
            result = subprocess.run(
                ["Rscript", str(test_file)],
                check=False,
                capture_output=True,
                text=True,
            )
        self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
