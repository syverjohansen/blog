from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/biathlon-predictions.yml"


class BiathlonPredictionWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_shadow_must_pass_before_publication(self) -> None:
        publish = self.source.split("  publish:\n", 1)[1]
        self.assertIn("needs: shadow", publish)
        self.assertIn("if: needs.shadow.outputs.ready == 'true'", publish)

    def test_combined_post_is_dispatched_after_fragment_publication(self) -> None:
        publication = self.source.index("name: Publish validated Biathlon predictions")
        dispatch = self.source.index("gh workflow run combined-race-picks.yml")
        self.assertLess(publication, dispatch)
        self.assertIn("actions: write", self.source)

    def test_publication_is_narrow_and_follows_site_validation(self) -> None:
        publication = self.source.index("git push origin HEAD:main")
        dispatch = self.source.index("gh workflow run combined-race-picks.yml")
        self.assertLess(publication, dispatch)
        self.assertIn('"data/biathlon/drafts/race-picks/$DATE_KEY"', self.source)
        self.assertIn(
            '"data/race-picks/fragments/$DATE_KEY/biathlon.md"', self.source
        )
        self.assertNotIn("git add .", self.source)


if __name__ == "__main__":
    unittest.main()
