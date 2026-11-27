import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/combined-race-picks.yml"


class CombinedRacePicksWorkflowTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text()

    def test_is_serialized_per_date_and_manual_only(self) -> None:
        self.assertIn("workflow_dispatch:", self.source)
        self.assertNotIn("schedule:", self.source)
        self.assertIn("combined-race-picks-${{ inputs.logical_date }}", self.source)
        self.assertIn("cancel-in-progress: false", self.source)

    def test_build_render_and_narrow_publication_are_ordered(self) -> None:
        build = self.source.index("race_picks_aggregation build")
        render = self.source.index("race_picks_aggregation render")
        publish = self.source.index("git add -A -- content/post/race-picks")
        self.assertLess(build, render)
        self.assertLess(render, publish)
        self.assertNotIn("data/race-picks/fragments", self.source[publish:])

    def test_night_before_render_includes_future_dated_post(self) -> None:
        aggregator = (ROOT / "pipelines/race_picks_aggregation.py").read_text()
        self.assertIn('"--buildFuture"', aggregator)


if __name__ == "__main__":
    unittest.main()
