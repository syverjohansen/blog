import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/cross-country-fantasyxc-evidence.yml"


class CrossCountryFantasyXCWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_full_fis_histories_are_checked_out_for_identity_only(self) -> None:
        self.assertIn(
            "/elo/python/ski/polars/excel365/all_men_scrape.csv", self.source
        )
        self.assertIn(
            "/elo/python/ski/polars/excel365/all_ladies_scrape.csv", self.source
        )
        self.assertIn(
            "--men-identity-history excel365/all_men_scrape.csv", self.source
        )
        self.assertIn(
            "--ladies-identity-history excel365/all_ladies_scrape.csv", self.source
        )

    def test_bounded_runtime_includes_transitive_quota_dependency(self) -> None:
        self.assertIn(
            "/elo/python/ski/polars/cross_country_world_cup_quotas.py",
            self.source,
        )

    def test_identity_histories_feed_resolver_and_guarded_promotion(self) -> None:
        resolver = self.source.index("python3 cross_country_fantasyxc_resolve.py")
        review = self.source.index("python3 cross_country_fantasyxc_review.py")
        resolver_block = self.source[resolver:review]
        self.assertEqual(resolver_block.count("--men-identity-history"), 1)
        self.assertEqual(resolver_block.count("--ladies-identity-history"), 1)
        self.assertNotIn("prediction-ledger", resolver_block.lower())

    def test_private_prediction_ledger_is_built_before_automatic_promotion(self) -> None:
        ledger = self.source.index("python3 cross_country_prediction_elo_pipeline.py")
        promote = self.source.index("python3 cross_country_fantasyxc_review.py auto-promote")
        self.assertLess(ledger, promote)
        self.assertIn('args+=(--prediction-ledger "$ledger")', self.source)
        self.assertIn("--allow-network", self.source[ledger:promote])
        ledger_step = self.source.index("name: Build private prediction Elo ledger")
        self.assertIn("continue-on-error: true", self.source[ledger_step:promote])

    def test_private_prediction_ledger_installs_its_html_parser(self) -> None:
        ledger_step = self.source.index("name: Build private prediction Elo ledger")
        promote = self.source.index("name: Apply guarded automatic roster policy")
        ledger_block = self.source[ledger_step:promote]
        self.assertIn(
            "python3 -m pip install --disable-pip-version-check beautifulsoup4",
            ledger_block,
        )
        self.assertNotIn("uv run", ledger_block)
        self.assertIn("python3 cross_country_prediction_elo_pipeline.py", ledger_block)

    def test_hosted_evidence_remains_shadow_only(self) -> None:
        self.assertIn('"publication": {"status": "disabled"}', self.source)
        self.assertNotIn("git push", self.source)

    def test_automatic_roster_is_saved_for_prediction_handoff(self) -> None:
        promote = self.source.index("name: Apply guarded automatic roster policy")
        stage = self.source.index("name: Stage automatic roster handoff")
        save = self.source.index("name: Save automatic roster handoff")
        self.assertLess(promote, stage)
        self.assertLess(stage, save)
        handoff = self.source[stage:save]
        self.assertIn("automated-announced-rosters.json", handoff)
        self.assertIn(".fantasyxc-handoff", handoff)
        self.assertIn("fis-prediction-ledger.csv", handoff)
        self.assertIn("fantasyxc-roster-v2-", self.source[save:])

    def test_successful_evidence_dispatches_prediction_chain(self) -> None:
        dispatch = self.source.index("name: Continue automatic prediction chain")
        self.assertIn("actions: write", self.source)
        self.assertIn("success()", self.source[dispatch:])
        self.assertIn("github.event_name == 'workflow_run'", self.source[dispatch:])
        self.assertIn(
            "gh workflow run cross-country-individual-predictions.yml",
            self.source[dispatch:],
        )
        self.assertIn('-f ski_ref="$SKI_SHA"', self.source[dispatch:])

    def test_notification_failure_does_not_block_prediction_handoff(self) -> None:
        email = self.source.index("name: Email changed roster digest")
        save = self.source.index("name: Save sent-digest marker")
        stage = self.source.index("name: Stage automatic roster handoff")
        self.assertLess(email, save)
        self.assertLess(save, stage)
        self.assertIn("continue-on-error: true", self.source[email:save])
        self.assertIn(
            "steps.roster_email.outcome == 'success'", self.source[save:stage]
        )

    def test_run_metadata_step_has_one_environment_mapping(self) -> None:
        metadata = self.source.split(
            "name: Record immutable run metadata", 1
        )[1].split("- name: Retain roster evidence", 1)[0]
        self.assertEqual(1, metadata.count("\n        env:\n"))
        self.assertIn("LEDGER_STATUS:", metadata)


if __name__ == "__main__":
    unittest.main()
