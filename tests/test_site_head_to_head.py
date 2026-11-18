from __future__ import annotations

import unittest

from pipelines.site_head_to_head import HeadToHeadError, build_head_to_head


def row(
    *, athlete_id: str, skier: str, season: str, race: str, date: str,
    city: str = "Ruka", distance: str = "10", technique: str = "C",
    ms: str = "0", place: str = "1", elo: str = "1400",
) -> dict[str, str]:
    return {
        "ID": athlete_id,
        "Skier": skier,
        "Season": season,
        "Race": race,
        "Date": date,
        "City": city,
        "Country": "Finland",
        "Distance": distance,
        "Technique": technique,
        "MS": ms,
        "Place": place,
        "Elo": elo,
    }


class SiteHeadToHeadTest(unittest.TestCase):
    def test_builds_chronological_athlete_rows_and_numeric_race_lookup(self) -> None:
        rows = [
            row(
                athlete_id="10", skier="Racer", season="2026", race="2",
                date="2026-01-02 00:00:00", place="2", elo="1412.5",
            ),
            row(
                athlete_id="10", skier="Racer", season="2025", race="0",
                date="2025-05-01", city="Summer",
            ),
            row(
                athlete_id="2", skier="Leader", season="2026", race="2",
                date="2026-01-02", place="1", elo="1450",
            ),
            row(
                athlete_id="10", skier="Racer", season="2026", race="1",
                date="2026-01-01", ms="", place="3", elo="1400",
            ),
        ]

        athletes, races = build_head_to_head(
            rows,
            race_fields={
                "Distance": "distance", "Technique": "technique", "MS": "ms"
            },
            athlete_defaults={"MS": "0"},
            lookup_field_names={"distance", "technique"},
        )

        self.assertEqual(list(athletes), ["10", "2"])
        self.assertEqual(
            [record["race"] for record in athletes["10"]], ["1", "2"]
        )
        self.assertEqual(athletes["10"][0]["ms"], "0")
        self.assertEqual(athletes["10"][1]["date"], "2026-01-02")
        self.assertEqual(athletes["10"][1]["place"], 2)
        self.assertEqual(athletes["10"][1]["elo"], 1412.5)
        self.assertEqual(
            races["2026_2"],
            {
                "season": 2026,
                "race": 2,
                "date": "2026-01-02",
                "city": "Ruka",
                "country": "Finland",
                "distance": "10",
                "technique": "C",
            },
        )

    def test_rejects_conflicting_metadata_for_one_race_key(self) -> None:
        rows = [
            row(
                athlete_id="1", skier="One", season="2026", race="1",
                date="2026-01-01", city="Ruka",
            ),
            row(
                athlete_id="2", skier="Two", season="2026", race="1",
                date="2026-01-01", city="Lahti",
            ),
        ]
        with self.assertRaisesRegex(HeadToHeadError, "conflicting metadata"):
            build_head_to_head(rows, race_fields={"Distance": "distance"})

    def test_rejects_missing_schema_and_invalid_numbers(self) -> None:
        with self.assertRaisesRegex(HeadToHeadError, "missing columns"):
            build_head_to_head([{"ID": "1"}], race_fields={"Distance": "distance"})
        invalid = row(
            athlete_id="1", skier="One", season="2026", race="1",
            date="2026-01-01", elo="not-a-number",
        )
        with self.assertRaisesRegex(HeadToHeadError, "invalid numeric value"):
            build_head_to_head([invalid], race_fields={"Distance": "distance"})


if __name__ == "__main__":
    unittest.main()
