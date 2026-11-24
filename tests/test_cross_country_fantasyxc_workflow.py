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

    def test_identity_histories_only_feed_the_roster_resolver(self) -> None:
        resolver = self.source.index("python3 cross_country_fantasyxc_resolve.py")
        review = self.source.index("python3 cross_country_fantasyxc_review.py")
        resolver_block = self.source[resolver:review]
        self.assertEqual(resolver_block.count("--men-identity-history"), 1)
        self.assertEqual(resolver_block.count("--ladies-identity-history"), 1)
        self.assertNotIn("prediction", resolver_block.lower())

    def test_hosted_evidence_remains_shadow_only(self) -> None:
        self.assertIn('"publication": {"status": "disabled"}', self.source)
        self.assertNotIn("git push", self.source)


if __name__ == "__main__":
    unittest.main()
