import importlib.util
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location(
    "sport_publication_smoke",
    REPO_ROOT / "pipelines" / "sport_publication_smoke.py",
)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class SportPublicationSmokeTests(unittest.TestCase):
    def test_contracts_cover_every_published_sport(self):
        self.assertEqual(
            set(MODULE.load_contracts(REPO_ROOT)),
            {"alpine", "cross-country", "biathlon", "skijump", "nordic-combined"},
        )

    def test_each_contract_has_publishable_data_and_prediction_fixture(self):
        for sport in MODULE.load_contracts(REPO_ROOT):
            with self.subTest(sport=sport):
                result = MODULE.validate_contract(REPO_ROOT, sport)
                self.assertEqual(result["sport"], sport)
                self.assertEqual(len(result["content_pages"]), 6)
                self.assertGreaterEqual(len(result["static_json"]), 13)
                self.assertTrue(result["prediction_references"])

    def test_nonempty_json_rejects_empty_payload(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "empty.json"
            path.write_text("[]\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "publication data is empty"):
                MODULE.load_nonempty_json(path)

    def test_unknown_sport_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "Unknown sport"):
            MODULE.validate_contract(REPO_ROOT, "snowball")


if __name__ == "__main__":
    unittest.main()
