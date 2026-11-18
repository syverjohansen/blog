import csv
import json
import tempfile
import unittest
from pathlib import Path

from pipelines.cross_country_site_data import (
    CURRENT_RATINGS,
    build_athlete_history_updates,
    write_site_data,
)

ROOT = Path(__file__).resolve().parents[1]
SKIER_TABLE = ROOT / "layouts/partials/cross-country/skier-table.html"

FIELDS = [
    *CURRENT_RATINGS.values(),
    "Date",
    "City",
    "Country",
    "Sex",
    "Distance",
    "Event",
    "MS",
    "Technique",
    "Place",
    "Skier",
    "Nation",
    "ID",
    "Season",
    "Race",
    "Birthday",
    "Age",
    "Exp",
    *CURRENT_RATINGS.keys(),
]


def row(
    *,
    sex: str,
    athlete_id: int,
    skier: str,
    season: int,
    race: int,
    event: str,
    place: int,
    pelo: float,
    elo: float,
    date: str,
) -> dict[str, object]:
    result: dict[str, object] = {
        "Date": date,
        "City": "Ruka",
        "Country": "Finland",
        "Sex": sex,
        "Distance": "10",
        "Event": event,
        "MS": 0,
        "Technique": "C",
        "Place": place,
        "Skier": skier,
        "Nation": "Norway",
        "ID": athlete_id,
        "Season": season,
        "Race": race,
        "Birthday": "2000-01-02T00:00:00",
        "Age": 26,
        "Exp": 3,
    }
    result.update({column: elo for column in CURRENT_RATINGS.values()})
    result.update({column: pelo for column in CURRENT_RATINGS})
    return result


def write_history(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


class CrossCountrySiteDataTests(unittest.TestCase):
    def test_athlete_page_merges_generated_history_overlay(self) -> None:
        source = SKIER_TABLE.read_text(encoding="utf-8")
        self.assertIn("function mergeHistoryUpdates", source)
        self.assertIn("_athlete_history_updates.json", source)
        self.assertIn("skierData = mergeHistoryUpdates", source)

    def test_athlete_updates_include_rollover_and_current_season(self) -> None:
        rows = [
            row(
                sex="M",
                athlete_id=1,
                skier="Racer",
                season=2026,
                race=0,
                event="Offseason",
                place=1,
                pelo=1420,
                elo=1420,
                date="2026-05-01 00:00:00",
            ),
            row(
                sex="M",
                athlete_id=1,
                skier="Racer",
                season=2027,
                race=1,
                event="World Cup",
                place=1,
                pelo=1402,
                elo=1412,
                date="2026-11-27",
            ),
        ]

        updates = build_athlete_history_updates(
            [{key: str(value) for key, value in item.items()} for item in rows],
            2027,
        )["1"]

        self.assertEqual(updates["Date"], ["2026-05-01 00:00:00", "2026-11-27"])
        self.assertEqual(updates["Season"], [2026, 2027])
        self.assertEqual(updates["Elo"], [1420.0, 1412.0])
        self.assertEqual(updates["Pelo"], [1420.0, 1402.0])
        self.assertEqual(updates["Elo_Pct"], [100.0, 100.0])

    def test_current_table_contains_only_current_season_racers(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            men = root / "men.csv"
            ladies = root / "ladies.csv"
            men_rows = [
                row(
                    sex="M",
                    athlete_id=1,
                    skier="Racer",
                    season=2026,
                    race=0,
                    event="Offseason",
                    place=1,
                    pelo=1420,
                    elo=1420,
                    date="2026-05-01 00:00:00",
                ),
                row(
                    sex="M",
                    athlete_id=2,
                    skier="Inactive",
                    season=2026,
                    race=0,
                    event="Offseason",
                    place=2,
                    pelo=1500,
                    elo=1500,
                    date="2026-05-01 00:00:00",
                ),
                row(
                    sex="M",
                    athlete_id=1,
                    skier="Racer",
                    season=2027,
                    race=1,
                    event="World Cup",
                    place=1,
                    pelo=1402,
                    elo=1412,
                    date="2026-11-27",
                ),
            ]
            ladies_rows = [
                row(
                    sex="L",
                    athlete_id=3,
                    skier="Lady",
                    season=2026,
                    race=0,
                    event="Offseason",
                    place=1,
                    pelo=1400,
                    elo=1400,
                    date="2026-05-01 00:00:00",
                ),
                row(
                    sex="L",
                    athlete_id=3,
                    skier="Lady",
                    season=2027,
                    race=1,
                    event="World Cup",
                    place=1,
                    pelo=1385,
                    elo=1395,
                    date="2026-11-27",
                ),
            ]
            write_history(men, men_rows)
            write_history(ladies, ladies_rows)

            output = root / "output"
            write_site_data(
                men_history=men,
                ladies_history=ladies,
                season=2027,
                output_root=output,
            )

            current = json.loads((output / "M_chrono_current.json").read_text())
            self.assertEqual([record["ID"] for record in current], [1])
            self.assertEqual(current[0]["Pelo"], 1412.0)
            self.assertEqual(current[0]["Distance_Pelo"], 1412.0)
            self.assertEqual(current[0]["Birthday"], "2000-01-02")

    def test_writes_undiscounted_race_zero_and_race_archive(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            men = root / "men.csv"
            ladies = root / "ladies.csv"
            for path, sex, athlete_id in ((men, "M", 1), (ladies, "L", 2)):
                write_history(
                    path,
                    [
                        row(
                            sex=sex,
                            athlete_id=athlete_id,
                            skier=f"{sex} Winner",
                            season=2026,
                            race=0,
                            event="Offseason",
                            place=1,
                            pelo=1420,
                            elo=1420,
                            date="2026-05-01 00:00:00",
                        ),
                        row(
                            sex=sex,
                            athlete_id=athlete_id,
                            skier=f"{sex} Winner",
                            season=2027,
                            race=1,
                            event="World Cup",
                            place=1,
                            pelo=1402,
                            elo=1412,
                            date="2026-11-27",
                        ),
                    ],
                )
            output = root / "output"

            write_site_data(
                men_history=men,
                ladies_history=ladies,
                season=2027,
                output_root=output,
            )

            summary = json.loads((output / "L/2027/summary.json").read_text())
            rollover = json.loads((output / "L/2027/race_0.json").read_text())
            results = json.loads((output / "L/2027/race_1.json").read_text())
            seasons = json.loads((output / "L/seasons.json").read_text())
            athlete_ids = json.loads((output / "L_all_ids.json").read_text())
            skier_info = json.loads((output / "L/skier_info.json").read_text())
            self.assertEqual(summary[0]["race_number"], 0)
            self.assertEqual(summary[0]["technique"], "C")
            self.assertEqual(rollover[0]["previous_elo"], 1420.0)
            self.assertEqual(rollover[0]["new_elo"], 1420.0)
            self.assertEqual(rollover[0]["change"], 0.0)
            self.assertEqual(results[0]["previous_elo"], 1402.0)
            self.assertEqual(results[0]["new_elo"], 1412.0)
            self.assertEqual(results[0]["change"], 10.0)
            self.assertEqual(
                seasons[-1], {"season": 2027, "race_count": 1, "winner": None}
            )
            self.assertEqual(athlete_ids, {"2": "L Winner"})
            self.assertEqual(
                skier_info,
                [
                    {
                        "id": 2,
                        "skier": "L Winner",
                        "sex": "L",
                        "nation": "nor",
                        "image": "/img/flags/nor.svg",
                    }
                ],
            )


if __name__ == "__main__":
    unittest.main()
