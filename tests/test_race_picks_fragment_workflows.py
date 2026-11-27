import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class RacePicksFragmentWorkflowTest(unittest.TestCase):
    def test_prediction_workflows_publish_fragments_then_dispatch_aggregation(self) -> None:
        contracts = {
            "alpine-production-shadow.yml": "alpine.md",
            "biathlon-predictions.yml": "biathlon.md",
            "cross-country-individual-predictions.yml": "cross-country-individual.md",
            "cross-country-team-predictions.yml": "cross-country-team.md",
            "nordic-combined-prediction-shadow.yml": "nordic-combined.md",
            "ski-jumping-prediction-shadow.yml": "ski-jumping.md",
        }
        for filename, fragment in contracts.items():
            with self.subTest(workflow=filename):
                source = (ROOT / ".github/workflows" / filename).read_text()
                publication = source.index("race-picks/fragments/")
                self.assertIn(fragment, source)
                dispatch = source.index("gh workflow run combined-race-picks.yml")
                self.assertLess(publication, dispatch)
                self.assertIn("actions: write", source)

    def test_sport_workflows_do_not_publish_public_race_pick_pages(self) -> None:
        for path in (ROOT / ".github/workflows").glob("*.yml"):
            if path.name == "combined-race-picks.yml":
                continue
            source = path.read_text()
            if "prediction" not in path.name and path.name != "alpine-production-shadow.yml":
                continue
            self.assertNotIn(
                'git add \\\n+            "content/post/race-picks/',
                source,
                path.name,
            )


if __name__ == "__main__":
    unittest.main()
