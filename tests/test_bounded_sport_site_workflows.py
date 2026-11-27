import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class BoundedSportSiteWorkflowTest(unittest.TestCase):
    def test_site_workflows_use_the_shared_bounded_render(self) -> None:
        workflows = {
            "biathlon-site-data-update.yml": "biathlon",
            "cross-country-site-data-update.yml": "cross-country",
            "nordic-combined-site-data-update.yml": "nordic-combined",
            "ski-jumping-site-data-update.yml": "skijump",
        }
        for filename, sport in workflows.items():
            with self.subTest(workflow=filename):
                source = (ROOT / ".github/workflows" / filename).read_text()
                self.assertIn("python3 -m pipelines.sport_site_smoke", source)
                self.assertIn(f"--sport {sport}", source)
                self.assertNotIn("run: hugo --minify", source)


if __name__ == "__main__":
    unittest.main()
