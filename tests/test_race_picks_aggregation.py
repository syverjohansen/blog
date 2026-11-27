import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.race_picks_aggregation import (
    RacePicksAggregationError,
    build_combined_post,
    fragment_path,
)


LOGICAL_DATE = date(2026, 11, 27)


def fragment(root: Path, name: str, heading: str, sport: str, reference: str) -> None:
    path = fragment_path(root, LOGICAL_DATE, name)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        "---\ntitle: Fragment\ndraft: false\n---\n\n"
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details.*\n\n"
        f"## {heading}\n\n### Event\n\n"
        f'{{{{< {sport}/datatable "{reference}" >}}}}\n'
    )
    data = root / "data" / f"{reference}.json"
    data.parent.mkdir(parents=True, exist_ok=True)
    data.write_text(json.dumps({"rows": [["Athlete", 1]]}))


class RacePicksAggregationTest(unittest.TestCase):
    def test_builds_one_ordered_post_and_removes_legacy_pages(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            fragment(root, "biathlon", "Biathlon", "biathlon", "biathlon/picks")
            fragment(root, "alpine", "Alpine", "alpine", "alpine/picks")
            fragment(
                root, "cross-country-individual", "Cross-Country",
                "cross-country", "cross-country/individual",
            )
            fragment(
                root, "cross-country-team", "Cross-Country",
                "cross-country", "cross-country/team",
            )
            legacy = root / "content/post/race-picks/biathlon-20261127.md"
            legacy.parent.mkdir(parents=True)
            legacy.write_text("legacy")

            report = build_combined_post(root, LOGICAL_DATE)
            post = root / report["post"]
            rendered = post.read_text()

            self.assertEqual(report["status"], "prepared")
            self.assertLess(rendered.index("## Alpine"), rendered.index("## Cross-Country"))
            self.assertLess(rendered.index("## Cross-Country"), rendered.index("## Biathlon"))
            self.assertEqual(rendered.count("## Cross-Country"), 1)
            self.assertEqual(rendered.count("Race Picks Methodology"), 1)
            self.assertFalse(legacy.exists())

    def test_fails_closed_for_missing_referenced_data(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            path = fragment_path(root, LOGICAL_DATE, "alpine")
            path.parent.mkdir(parents=True)
            path.write_text(
                "---\ntitle: Fragment\n---\n\n## Alpine\n\n"
                '{{< alpine/datatable "alpine/missing" >}}\n'
            )
            with self.assertRaisesRegex(RacePicksAggregationError, "missing"):
                build_combined_post(root, LOGICAL_DATE)

    def test_fails_closed_without_fragments(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            with self.assertRaisesRegex(RacePicksAggregationError, "no prediction"):
                build_combined_post(Path(directory), LOGICAL_DATE)


if __name__ == "__main__":
    unittest.main()
