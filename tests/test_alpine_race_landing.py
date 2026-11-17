import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class AlpineRaceLandingTest(unittest.TestCase):
    def test_active_season_is_not_labelled_as_missing_a_winner(self) -> None:
        template = (
            ROOT / "layouts/partials/alpine/races-landing.html"
        ).read_text(encoding="utf-8")

        self.assertIn(
            "const isActiveSeason = season.season === seasons[0].season;",
            template,
        )
        self.assertEqual(template.count("Season in progress"), 2)
        self.assertIn("Ladies${season.ladiesWinner ? ' Winner' : ''}", template)
        self.assertIn("Men's${season.mensWinner ? ' Winner' : ''}", template)


if __name__ == "__main__":
    unittest.main()
