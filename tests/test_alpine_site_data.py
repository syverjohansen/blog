import csv
import json
import tempfile
import unittest
from pathlib import Path

from pipelines.alpine_site_data import (
    CURRENT_RATINGS,
    build_athlete_history_updates,
    write_site_data,
)


FIELDS = [
    "Date", "City", "Country", "Sex", "Distance", "Event", "Place", "Skier",
    "Nation", "ID", "Season", "Race", "Birthday", "Pelo", "Elo",
    *[column for column in CURRENT_RATINGS.values() if column != "Elo"],
]


def row(*, sex: str, athlete_id: int, skier: str, season: int, race: int,
        event: str, place: int, pelo: float, elo: float, date: str) -> dict[str, object]:
    result: dict[str, object] = {
        "Date": date, "City": "Levi", "Country": "Finland", "Sex": sex,
        "Distance": "Slalom", "Event": event, "Place": place, "Skier": skier,
        "Nation": "Norway", "ID": athlete_id, "Season": season, "Race": race,
        "Birthday": "2000-01-02T00:00:00", "Pelo": pelo, "Elo": elo,
    }
    result.update({column: elo for column in CURRENT_RATINGS.values() if column != "Elo"})
    return result


def write_history(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


class AlpineSiteDataTest(unittest.TestCase):
    def test_athlete_updates_replace_rollover_and_append_new_season(self) -> None:
        rows = [
            row(sex="M", athlete_id=1, skier="Racer", season=2026, race=40,
                event="World Cup", place=1, pelo=1400, elo=1420,
                date="2026-03-25"),
            row(sex="M", athlete_id=1, skier="Racer", season=2026, race=0,
                event="Offseason", place=1, pelo=1420, elo=1402,
                date="2026-05-01"),
            row(sex="M", athlete_id=1, skier="Racer", season=2027, race=1,
                event="World Cup", place=1, pelo=1402, elo=1412,
                date="2026-10-25"),
        ]

        updates = build_athlete_history_updates(
            [{key: str(value) for key, value in item.items()} for item in rows],
            2027,
        )["1"]

        self.assertEqual(updates["Date"], ["2026-05-01", "2026-10-25"])
        self.assertEqual(updates["Season"], [2026, 2027])
        self.assertEqual(updates["Race"], [0, 1])
        self.assertEqual(updates["Elo"], [1402.0, 1412.0])
        self.assertEqual(updates["Pelo"], [1420.0, 1402.0])
        self.assertEqual(updates["Elo_Pct"], [100.0, 100.0])

    def test_current_table_uses_roster_plus_new_racers_and_post_race_elo(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            men = root / "men.csv"
            ladies = root / "ladies.csv"
            men_rows = [
                row(sex="M", athlete_id=1, skier="Racer", season=2026, race=0,
                    event="Offseason", place=1, pelo=1400, elo=1385, date="2026-05-01"),
                row(sex="M", athlete_id=2, skier="Inactive", season=2026, race=0,
                    event="Offseason", place=2, pelo=1350, elo=1342.5, date="2026-05-01"),
                row(sex="M", athlete_id=1, skier="Racer", season=2027, race=1,
                    event="World Cup", place=1, pelo=1385, elo=1400, date="2026-10-25"),
                row(sex="M", athlete_id=3, skier="Returner", season=2027, race=1,
                    event="World Cup", place=2, pelo=1300, elo=1310, date="2026-10-25"),
            ]
            ladies_rows = [
                row(sex="L", athlete_id=4, skier="Lady", season=2026, race=0,
                    event="Offseason", place=1, pelo=1400, elo=1385, date="2026-05-01"),
                row(sex="L", athlete_id=4, skier="Lady", season=2027, race=1,
                    event="World Cup", place=1, pelo=1385, elo=1395, date="2026-10-24"),
            ]
            write_history(men, men_rows)
            write_history(ladies, ladies_rows)
            output = root / "output"

            write_site_data(
                men_history=men, ladies_history=ladies, season=2027, output_root=output
            )

            current = json.loads((output / "M_chrono_current.json").read_text())
            all_time = json.loads((output / "M_chrono_all_time.json").read_text())
            self.assertEqual([record["ID"] for record in current], [1, 3])
            self.assertEqual(current[0]["Pelo"], 1400.0)
            self.assertEqual(current[1]["Pelo"], 1310.0)
            self.assertEqual(current[0]["Birthday"], "2000-01-02")
            self.assertEqual([record["ID"] for record in all_time], [1, 2, 3])
            self.assertEqual(all_time[0]["Overall"], 1400.0)
            self.assertEqual(all_time[0]["Overall_Date"], "2026-10-25")
            self.assertEqual(all_time[0]["Slalom"], 1400.0)
            self.assertEqual(all_time[0]["Slalom_Date"], "2026-10-25")

    def test_writes_target_season_archive_and_index(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            men = root / "men.csv"
            ladies = root / "ladies.csv"
            for path, sex, athlete_id in ((men, "M", 1), (ladies, "L", 2)):
                write_history(path, [
                    row(sex=sex, athlete_id=athlete_id, skier=f"{sex} Winner", season=2026,
                        race=0, event="Offseason", place=1, pelo=1400, elo=1385,
                        date="2026-05-01"),
                    row(sex=sex, athlete_id=99, skier=f"{sex} Inactive", season=2026,
                        race=0, event="Offseason", place=2, pelo=1500, elo=1470,
                        date="2026-05-01"),
                    row(sex=sex, athlete_id=athlete_id, skier=f"{sex} Winner", season=2027,
                        race=1, event="World Cup", place=1, pelo=1385, elo=1395,
                        date="2026-10-24"),
                    row(sex=sex, athlete_id=100, skier=f"{sex} Newcomer", season=2027,
                        race=1, event="World Cup", place=2, pelo=1300, elo=1305,
                        date="2026-10-24"),
                ])
            output = root / "output"

            write_site_data(
                men_history=men, ladies_history=ladies, season=2027, output_root=output
            )

            summary = json.loads((output / "L/2027/summary.json").read_text())
            rollover = json.loads((output / "L/2027/race_0.json").read_text())
            results = json.loads((output / "L/2027/race_1.json").read_text())
            seasons = json.loads((output / "L/seasons.json").read_text())
            updates = json.loads(
                (output / "L_athlete_history_updates.json").read_text()
            )
            athlete_ids = json.loads((output / "L_all_ids.json").read_text())
            skier_info = json.loads((output / "L/skier_info.json").read_text())
            skier_lookup = json.loads((output / "L_skiers_lookup.json").read_text())
            skier_group = json.loads(
                (output / "L/skiers_0_to_499_0.json").read_text()
            )
            head_to_head = json.loads(
                (output / "head2head/L_head2head_data.json").read_text()
            )
            race_lookup = json.loads(
                (output / "head2head/L_race_lookup.json").read_text()
            )
            self.assertEqual(summary[0]["podium"][0]["name"], "L Winner")
            self.assertEqual(summary[0]["race_number"], 0)
            self.assertEqual(summary[0]["event"], "Offseason")
            self.assertEqual(rollover[0]["previous_elo"], 1400.0)
            self.assertEqual(rollover[0]["new_elo"], 1385.0)
            self.assertEqual(rollover[0]["change"], -15.0)
            self.assertEqual([record["id"] for record in rollover], [2, 100])
            self.assertEqual(rollover[1]["previous_elo"], 1300.0)
            self.assertEqual(rollover[1]["new_elo"], 1300.0)
            self.assertEqual(results[0]["previous_elo"], 1385.0)
            self.assertEqual(results[0]["new_elo"], 1395.0)
            self.assertEqual(results[0]["change"], 10.0)
            self.assertEqual(seasons[-1], {"season": 2027, "race_count": 1, "winner": None})
            self.assertEqual(updates["2"]["Date"], ["2026-05-01", "2026-10-24"])
            self.assertEqual(updates["2"]["Elo"], [1385.0, 1395.0])
            self.assertEqual(
                athlete_ids,
                {"2": "L Winner", "99": "L Inactive", "100": "L Newcomer"},
            )
            self.assertEqual(
                [record["id"] for record in skier_info],
                [2, 99, 100],
            )
            self.assertEqual(skier_info[0]["nation"], "nor")
            self.assertEqual(skier_info[0]["image"], "/img/flags/nor.svg")
            self.assertEqual(skier_lookup[0]["group"], "0_to_499_0")
            self.assertEqual(skier_lookup[0]["nation_code"], "nor")
            self.assertEqual(skier_group["2"]["Race"], [0, 1])
            self.assertEqual(skier_group["2"]["Elo"], [1385.0, 1395.0])
            self.assertAlmostEqual(
                skier_group["2"]["Elo_Pct"][0], 1385 / 1470 * 100
            )
            self.assertEqual(skier_group["2"]["Elo_Pct"][1], 100.0)
            self.assertEqual(head_to_head["2"][0]["season"], "2027")
            self.assertEqual(head_to_head["2"][0]["race"], "1")
            self.assertEqual(head_to_head["2"][0]["event"], "Slalom")
            self.assertNotIn("99", head_to_head)
            self.assertEqual(race_lookup["2027_1"]["season"], 2027)
            self.assertEqual(race_lookup["2027_1"]["event"], "Slalom")


if __name__ == "__main__":
    unittest.main()
