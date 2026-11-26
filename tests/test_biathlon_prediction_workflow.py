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

    def test_generated_post_is_rendered_without_the_full_historical_site(self) -> None:
        render = self.source.index("name: Render targeted Biathlon post candidate")
        publication = self.source.index(
            "name: Publish validated Biathlon predictions", render
        )
        render_step = self.source[render:publication]
        self.assertIn("pipelines.biathlon_prediction_site_smoke", render_step)
        self.assertIn('biathlon-$DATE_KEY.md', render_step)
        self.assertIn('biathlon-site-smoke.json', render_step)

    def test_publication_is_narrow_and_follows_site_validation(self) -> None:
        render = self.source.index("pipelines.biathlon_prediction_site_smoke")
        publication = self.source.index("git push origin HEAD:main")
        self.assertLess(render, publication)
        self.assertIn('"data/biathlon/drafts/race-picks/$DATE_KEY"', self.source)
        self.assertIn(
            '"content/post/race-picks/biathlon-$DATE_KEY.md"', self.source
        )
        self.assertNotIn("git add .", self.source)


if __name__ == "__main__":
    unittest.main()
