from __future__ import annotations

import unittest

from pipelines.site_all_time import AllTimeEloError, build_all_time_elo


def row(
    *, athlete_id: str, skier: str, nation: str, date: str,
    season: str, race: str, elo: str, discipline: str,
) -> dict[str, str]:
    return {
        "ID": athlete_id,
        "Skier": skier,
        "Nation": nation,
        "Date": date,
        "Season": season,
        "Race": race,
        "Elo": elo,
        "Discipline_Elo": discipline,
    }


class SiteAllTimeEloTest(unittest.TestCase):
    def test_builds_maxima_dates_latest_identity_and_stable_order(self) -> None:
        rows = [
            row(
                athlete_id="20", skier="Old Name", nation="Sweden",
                date="2025-01-01", season="2025", race="1",
                elo="1500", discipline="1400",
            ),
            row(
                athlete_id="3", skier="Leader", nation="USA",
                date="2026-01-01", season="2026", race="1",
                elo="1600", discipline="1500",
            ),
            row(
                athlete_id="20", skier="Current Name", nation="Norway",
                date="2026-01-02", season="2026", race="2",
                elo="1500", discipline="1450",
            ),
        ]

        records = build_all_time_elo(
            rows, {"Elo": "Overall", "Discipline_Elo": "Discipline"}
        )

        self.assertEqual([record["ID"] for record in records], [3, 20])
        self.assertEqual([record["Place"] for record in records], [1, 2])
        self.assertEqual(records[1]["Skier"], "Current Name")
        self.assertEqual(records[1]["Nation"], "Norway")
        self.assertEqual(records[1]["Overall"], 1500.0)
        self.assertEqual(records[1]["Overall_Date"], "2025-01-01")
        self.assertEqual(records[1]["Discipline"], 1450.0)
        self.assertEqual(records[1]["Discipline_Date"], "2026-01-02")
        self.assertEqual(
            build_all_time_elo(
                list(reversed(rows)),
                {"Elo": "Overall", "Discipline_Elo": "Discipline"},
            ),
            records,
        )

    def test_ignores_missing_and_non_finite_ratings(self) -> None:
        rows = [
            row(
                athlete_id="1", skier="Racer", nation="Norway",
                date="2026-01-01", season="2026", race="1",
                elo="1400", discipline="inf",
            ),
            row(
                athlete_id="1", skier="Racer", nation="Norway",
                date="2026-01-02", season="2026", race="2",
                elo="nan", discipline="",
            ),
        ]

        record = build_all_time_elo(
            rows, {"Elo": "Overall", "Discipline_Elo": "Discipline"}
        )[0]
        self.assertEqual(record["Overall"], 1400.0)
        self.assertIsNone(record["Discipline"])
        self.assertIsNone(record["Discipline_Date"])

    def test_rounds_ratings_to_the_legacy_json_precision(self) -> None:
        rows = [
            row(
                athlete_id="1", skier="Racer", nation="Norway",
                date="2026-01-01", season="2026", race="1",
                elo="1400.123456789123", discipline="1300",
            )
        ]
        record = build_all_time_elo(
            rows, {"Elo": "Overall", "Discipline_Elo": "Discipline"}
        )[0]
        self.assertEqual(record["Overall"], 1400.1234567891)

    def test_rejects_missing_schema_and_invalid_identity(self) -> None:
        with self.assertRaisesRegex(AllTimeEloError, "missing columns"):
            build_all_time_elo(
                [{"ID": "1"}], {"Elo": "Overall"}
            )
        invalid = row(
            athlete_id="bad", skier="Racer", nation="Norway",
            date="2026-01-01", season="2026", race="1",
            elo="1400", discipline="1300",
        )
        with self.assertRaisesRegex(AllTimeEloError, "invalid athlete ID"):
            build_all_time_elo(
                [invalid], {"Elo": "Overall", "Discipline_Elo": "Discipline"}
            )

        invalid_rating = row(
            athlete_id="1", skier="Racer", nation="Norway",
            date="2026-01-01", season="2026", race="1",
            elo="not-a-rating", discipline="1300",
        )
        with self.assertRaisesRegex(AllTimeEloError, "invalid rating"):
            build_all_time_elo(
                [invalid_rating],
                {"Elo": "Overall", "Discipline_Elo": "Discipline"},
            )


if __name__ == "__main__":
    unittest.main()
