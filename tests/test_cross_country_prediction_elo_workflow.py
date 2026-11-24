import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-prediction-elo-shadow.yml"


class CrossCountryPredictionEloWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_is_manual_read_only_and_never_publishes(self) -> None:
        trigger = self.source.split("permissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertNotIn("workflow_run:", trigger)
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertIn('"publication": {"status": "disabled"}', self.source)
        self.assertNotIn("git push", self.source)
        self.assertNotIn("BLOG_AUTOMATION_TOKEN", self.source)
        self.assertNotIn(" install \\", self.source)

    def test_resolves_and_checks_out_exact_bounded_ski_revision(self) -> None:
        self.assertIn('gh api "repos/syverjohansen/ski/commits/$SKI_REF"', self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertIn("persist-credentials: false", self.source)
        for required in (
            "cross_country_prediction_elo_pipeline.py",
            "cross_country_prediction_elo_shadow.py",
            "cross_country_weekend_inputs.py",
            "cross_country_results_gap.py",
            "schedule_sources.py",
            "cross_country_prediction_elo_policy.json",
        ):
            self.assertIn(required, self.source)

    def test_uses_official_prediction_shadow_bundle_as_gate(self) -> None:
        bundle = self.source.index("name: Build official weekend comparison bundle")
        inspect = self.source.index("name: Inspect official comparison eligibility")
        ledger = self.source.index("name: Build current private K4 prediction ledger")
        self.assertLess(bundle, inspect)
        self.assertLess(inspect, ledger)
        self.assertIn("--prediction-shadow", self.source[bundle:inspect])
        self.assertIn('source_mode") == "official_fis_startlist"', self.source)
        self.assertIn("steps.bundle.outputs.official_ready == 'true'", self.source)

    def test_network_is_limited_to_private_ledger_stage(self) -> None:
        self.assertEqual(self.source.count("--allow-network"), 1)
        ledger = self.source.index("name: Build current private K4 prediction ledger")
        overlay = self.source.index("name: Build private rating overlays")
        self.assertIn("--allow-network", self.source[ledger:overlay])

    def test_materializes_baseline_and_overlay_candidate_separately(self) -> None:
        materialize = self.source.split(
            "name: Materialize identical baseline and K4 runtimes", 1
        )[1].split("- name: Set up R", 1)[0]
        self.assertEqual(
            materialize.count("pipelines.cross_country_weekend_consumer"), 2
        )
        self.assertEqual(materialize.count("--prediction-elo-root"), 1)
        self.assertIn('--output-dir "$BASELINE_RUNTIME"', materialize)
        self.assertIn('--output-dir "$CANDIDATE_RUNTIME"', materialize)

    def test_runs_same_model_with_same_seed_then_compares(self) -> None:
        baseline = self.source.index("name: Run baseline simulation")
        candidate = self.source.index("name: Run K4 candidate simulation")
        validate = self.source.index("name: Validate both shadow outputs")
        compare = self.source.index(
            "name: Compare baseline and K4 full prediction pools"
        )
        self.assertLess(baseline, candidate)
        self.assertLess(candidate, validate)
        self.assertLess(validate, compare)
        simulation_region = self.source[baseline:validate]
        self.assertEqual(
            simulation_region.count("NORDIC_RANDOM_SEED: ${{ env.RANDOM_SEED }}"),
            2,
        )
        self.assertEqual(
            simulation_region.count(
                "Rscript content/post/cross-country/drafts/weekly-picks-simulation.R"
            ),
            2,
        )
        self.assertIn(
            "pipelines.cross_country_prediction_elo_comparison",
            self.source[compare:],
        )

    def test_always_retains_complete_evidence(self) -> None:
        upload = self.source.split("name: Retain complete comparison evidence", 1)[1]
        self.assertIn("if: always()", upload)
        self.assertIn("retention-days: 30", upload)
        self.assertIn("${{ runner.temp }}/cross-country-prediction-elo-shadow/", upload)


if __name__ == "__main__":
    unittest.main()
