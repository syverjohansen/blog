from __future__ import annotations

import unittest

from pipelines.site_athletes import (
    AthleteMetadataError,
    athlete_group_key,
    build_athlete_index,
    build_grouped_athlete_histories,
    build_skier_info,
    build_skier_lookup,
)


def row(
    *,
    athlete_id: str,
    skier: str,
    nation: str,
    date: str,
    season: str,
    race: str,
) -> dict[str, str]:
    return {
        "ID": athlete_id,
        "Skier": skier,
        "Nation": nation,
        "Date": date,
        "Season": season,
        "Race": race,
    }


class SiteAthletesTest(unittest.TestCase):
    def test_preserves_legacy_group_boundaries(self) -> None:
        self.assertEqual(athlete_group_key(1), "0_to_499_0")
        self.assertEqual(athlete_group_key(499), "0_to_499_400")
        self.assertEqual(athlete_group_key(500), "500_to_999_500")
        self.assertEqual(athlete_group_key(999), "500_to_999_900")
        self.assertEqual(athlete_group_key(1000), "1000_plus_1000")
        self.assertEqual(athlete_group_key(1499), "1000_plus_1000")
        self.assertEqual(athlete_group_key(1500), "1000_plus_1500")

    def test_uses_latest_identity_and_orders_numeric_ids(self) -> None:
        rows = [
            row(
                athlete_id="20",
                skier="Old Name",
                nation="Sweden",
                date="2025-03-01",
                season="2025",
                race="20",
            ),
            row(
                athlete_id="3",
                skier="Earlier ID",
                nation="USA",
                date="2026-01-01",
                season="2026",
                race="1",
            ),
            row(
                athlete_id="20",
                skier="Current Name",
                nation="Norway",
                date="2026-02-01",
                season="2026",
                race="2",
            ),
        ]

        self.assertEqual(
            build_athlete_index(rows),
            {"3": "Earlier ID", "20": "Current Name"},
        )
        self.assertEqual(
            build_skier_info(rows, "M"),
            [
                {
                    "id": 3,
                    "skier": "Earlier ID",
                    "sex": "M",
                    "nation": "usa",
                    "image": "/img/flags/usa.svg",
                },
                {
                    "id": 20,
                    "skier": "Current Name",
                    "sex": "M",
                    "nation": "nor",
                    "image": "/img/flags/nor.svg",
                },
            ],
        )
        self.assertEqual(
            build_skier_lookup(rows),
            [
                {
                    "id": 3,
                    "skier": "Earlier ID",
                    "nation": "USA",
                    "nation_code": "usa",
                    "group": "0_to_499_0",
                },
                {
                    "id": 20,
                    "skier": "Current Name",
                    "nation": "Norway",
                    "nation_code": "nor",
                    "group": "0_to_499_0",
                },
            ],
        )

    def test_builds_column_oriented_histories_and_race_percentages(self) -> None:
        rows = [
            {
                **row(
                    athlete_id="20", skier="Racer", nation="Norway",
                    date="2026-01-02", season="2026", race="1",
                ),
                "Place": "2", "Elo": "1400", "Age": "24.5",
            },
            {
                **row(
                    athlete_id="20", skier="Racer", nation="Norway",
                    date="2026-01-01", season="2026", race="0",
                ),
                "Place": "1", "Elo": "1350", "Age": "24.5",
            },
            {
                **row(
                    athlete_id="501", skier="Leader", nation="Sweden",
                    date="2026-01-02", season="2026", race="1",
                ),
                "Place": "1", "Elo": "1600", "Age": "25",
            },
        ]

        groups = build_grouped_athlete_histories(
            rows,
            elo_columns=["Elo"],
            integer_columns={"ID", "Season", "Race", "Place"},
            float_columns={"Elo", "Age"},
        )

        history = groups["0_to_499_0"]["20"]
        self.assertEqual(history["Date"], ["2026-01-01", "2026-01-02"])
        self.assertEqual(history["Race"], [0, 1])
        self.assertEqual(history["Elo"], [1350.0, 1400.0])
        self.assertEqual(history["Elo_Pct"], [100.0, 87.5])
        self.assertEqual(groups["500_to_999_500"]["501"]["Elo_Pct"], [100.0])

    def test_rejects_unknown_nations_instead_of_publishing_broken_flags(self) -> None:
        rows = [
            row(
                athlete_id="1",
                skier="Racer",
                nation="Unknown Place",
                date="2026-01-01",
                season="2026",
                race="1",
            )
        ]
        with self.assertRaisesRegex(AthleteMetadataError, "unsupported nation"):
            build_skier_info(rows, "L")

    def test_rejects_invalid_ids_and_empty_names(self) -> None:
        invalid_id = [
            row(
                athlete_id="bad",
                skier="Racer",
                nation="Norway",
                date="2026-01-01",
                season="2026",
                race="1",
            )
        ]
        with self.assertRaisesRegex(AthleteMetadataError, "invalid athlete ID"):
            build_athlete_index(invalid_id)

        empty_name = [
            row(
                athlete_id="1",
                skier=" ",
                nation="Norway",
                date="2026-01-01",
                season="2026",
                race="1",
            )
        ]
        with self.assertRaisesRegex(AthleteMetadataError, "no display name"):
            build_athlete_index(empty_name)


if __name__ == "__main__":
    unittest.main()
