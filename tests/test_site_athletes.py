from __future__ import annotations

import unittest

from pipelines.site_athletes import (
    AthleteMetadataError,
    build_athlete_index,
    build_skier_info,
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
