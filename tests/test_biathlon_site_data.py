import csv
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.biathlon_site_data import CURRENT_RATINGS, write_site_data


FIELDS = [
    "Date", "City", "Country", "Sex", "Distance", "RaceType", "MassStart",
    "Event", "Place", "Skier", "Nation", "ID", "Season", "Race",
    "Birthday", "Age", "Exp", "Leg", *CURRENT_RATINGS.values(),
    *CURRENT_RATINGS,
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
    race_type: str = "Individual",
    distance: str = "10",
    mass_start: int = 0,
) -> dict[str, object]:
    result: dict[str, object] = {
        "Date": date,
        "City": "Lillehammer",
        "Country": "Norway",
        "Sex": sex,
        "Distance": distance,
        "RaceType": race_type,
        "MassStart": mass_start,
        "Event": event,
        "Place": place,
        "Skier": skier,
        "Nation": "Norway",
        "ID": athlete_id,
        "Season": season,
        "Race": race,
        "Birthday": "2000-01-02T00:00:00.000000",
        "Age": 26,
        "Exp": 3,
        "Leg": 0,
        "Pelo": pelo,
        "Elo": elo,
    }
    for previous, current in CURRENT_RATINGS.items():
        if previous == "Pelo":
            continue
        result[previous] = pelo
        result[current] = elo
    return result


def write_history(path: Path, rows: list[dict[str, object]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


class BiathlonSiteDataTest(unittest.TestCase):
    def test_active_season_uses_only_actual_racers_and_latest_elo(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            men = root / "men.csv"
            ladies = root / "ladies.csv"
            men_rows = [
                row(
                    sex="M", athlete_id=1, skier="Active", season=2026,
                    race=0, event="Offseason", place=1, pelo=1500, elo=1470,
                    date="2026-05-01", race_type="Offseason", distance="0",
                ),
                row(
                    sex="M", athlete_id=2, skier="Inactive", season=2026,
                    race=0, event="Offseason", place=2, pelo=1600, elo=1555,
                    date="2026-05-01", race_type="Offseason", distance="0",
                ),
                row(
                    sex="M", athlete_id=1, skier="Active", season=2027,
                    race=1, event="World Cup", place=1, pelo=1470, elo=1475,
                    date="2026-11-20", race_type="Relay",
                ),
                row(
                    sex="M", athlete_id=1, skier="Active", season=2027,
                    race=2, event="World Cup", place=2, pelo=1475, elo=1480,
                    date="2026-11-21",
                ),
                row(
                    sex="M", athlete_id=1, skier="Active", season=2027,
                    race=3, event="World Cup", place=1, pelo=1480, elo=1495,
                    date="2026-11-22",
                ),
                row(
                    sex="M", athlete_id=3, skier="Newcomer", season=2027,
                    race=2, event="World Cup", place=1, pelo=1300, elo=1310,
                    date="2026-11-21",
                ),
                row(
                    sex="M", athlete_id=1, skier="Active", season=2027,
                    race=0, event="Offseason", place=1, pelo=1495, elo=1465.75,
                    date="2027-05-01", race_type="Offseason", distance="0",
                ),
            ]
            # A skier can establish an event rating in their first race;
            # Race 0 compares only overall Elo and must not require a prior
            # category-specific rating.
            men_rows[5]["Individual_Pelo"] = ""
            ladies_rows = [
                row(
                    sex="L", athlete_id=4, skier="Lady", season=2026,
                    race=0, event="Offseason", place=1, pelo=1400, elo=1385,
                    date="2026-05-01", race_type="Offseason", distance="0",
                ),
                row(
                    sex="L", athlete_id=4, skier="Lady", season=2026,
                    race=1, event="World Cup", place=1, pelo=1385, elo=1405,
                    date="2025-11-20",
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
                as_of=date(2026, 11, 25),
                men_ranking_history=men,
                ladies_ranking_history=ladies,
            )

            current = json.loads((output / "M_chrono_current.json").read_text())
            all_time = json.loads((output / "M_chrono_all_time.json").read_text())
            rollover = json.loads((output / "M/2027/race_0.json").read_text())
            summary = json.loads((output / "M/2027/summary.json").read_text())
            updates = json.loads(
                (output / "M_athlete_history_updates.json").read_text()
            )
            athlete_ids = json.loads((output / "M_all_ids.json").read_text())
            skier_lookup = json.loads(
                (output / "M_skiers_lookup.json").read_text()
            )
            skier_info = json.loads((output / "M/skier_info.json").read_text())
            skier_group = json.loads(
                (output / "M/skiers_0_to_499_0.json").read_text()
            )
            head_to_head = json.loads(
                (output / "head2head/M_head2head_data.json").read_text()
            )
            race_lookup = json.loads(
                (output / "head2head/M_race_lookup.json").read_text()
            )
            rankings = json.loads((output / "M/ranks.json").read_text())
            ladies_current = json.loads(
                (output / "L_chrono_current.json").read_text()
            )
            ladies_summary = json.loads(
                (output / "L/2027/summary.json").read_text()
            )
            ladies_rollover = json.loads(
                (output / "L/2027/race_0.json").read_text()
            )

            self.assertEqual([record["ID"] for record in current], [1, 3])
            self.assertEqual(current[0]["Pelo"], 1495.0)
            self.assertEqual(current[0]["Individual_Pelo"], 1495.0)
            self.assertEqual(current[0]["Birthday"], "2000-01-02")
            self.assertEqual(ladies_current, [])
            self.assertEqual(ladies_summary, [])
            self.assertEqual(ladies_rollover, [])
            self.assertEqual([record["id"] for record in rollover], [1, 3])
            self.assertEqual(rollover[0]["previous_elo"], 1500.0)
            self.assertEqual(rollover[0]["new_elo"], 1495.0)
            self.assertEqual(rollover[1]["previous_elo"], 1300.0)
            self.assertEqual(rollover[1]["new_elo"], 1310.0)
            self.assertEqual(summary[0]["date"], "2027-05-01")
            self.assertEqual(summary[0]["racetype"], "Offseason")
            self.assertEqual(
                [
                    (record["race_number"], record["date"], record["racetype"])
                    for record in summary[1:]
                ],
                [
                    (1, "2026-11-20", "Relay"),
                    (2, "2026-11-21", "Individual"),
                    (3, "2026-11-22", "Individual"),
                ],
            )
            self.assertEqual(all_time[0]["ID"], 2)
            self.assertEqual(all_time[0]["Overall"], 1555.0)
            self.assertEqual(all_time[0]["Overall_Date"], "2026-05-01")
            self.assertEqual(
                updates["1"]["Date"],
                [
                    "2026-05-01",
                    "2026-11-20",
                    "2026-11-21",
                    "2026-11-22",
                    "2027-05-01",
                ],
            )
            self.assertEqual(
                athlete_ids,
                {"1": "Active", "2": "Inactive", "3": "Newcomer"},
            )
            self.assertEqual(skier_lookup[0]["group"], "0_to_499_0")
            self.assertEqual(skier_lookup[0]["nation_code"], "nor")
            self.assertEqual(skier_info[0]["image"], "/img/flags/nor.svg")
            self.assertEqual(skier_group["1"]["Race"], [0, 1, 2, 3, 0])
            self.assertEqual(skier_group["1"]["MassStart"], [0, 0, 0, 0, 0])
            self.assertEqual(len(head_to_head["1"]), 3)
            self.assertEqual(head_to_head["1"][0]["event"], "Relay")
            self.assertEqual(race_lookup["2027_1"]["event"], "Relay")
            self.assertEqual(rankings[0]["Skier"], "Active")
            self.assertEqual(rankings[0]["WC"], 14.0)
            self.assertEqual(rankings[0]["Table"], 0.0)

    def test_completed_season_winner_uses_undiscounted_pelo(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for filename, sex, athlete_id in (
                ("men.csv", "M", 1),
                ("ladies.csv", "L", 2),
            ):
                write_history(
                    root / filename,
                    [
                        row(
                            sex=sex, athlete_id=athlete_id, skier="Winner",
                            season=2025, race=0, event="Offseason", place=1,
                            pelo=1400, elo=1385, date="2025-05-01",
                            race_type="Offseason", distance="0",
                        ),
                        row(
                            sex=sex, athlete_id=athlete_id, skier="Winner",
                            season=2026, race=1, event="World Cup", place=1,
                            pelo=1385, elo=1500, date="2025-11-22",
                        ),
                        row(
                            sex=sex, athlete_id=3, skier="Runner-up",
                            season=2026, race=1, event="World Cup", place=2,
                            pelo=1450, elo=1490, date="2025-11-22",
                        ),
                        row(
                            sex=sex, athlete_id=athlete_id, skier="Winner",
                            season=2026, race=0, event="Offseason", place=1,
                            pelo=1500, elo=1470, date="2026-05-01",
                            race_type="Offseason", distance="0",
                        ),
                        row(
                            sex=sex, athlete_id=3, skier="Runner-up",
                            season=2026, race=0, event="Offseason", place=2,
                            pelo=1490, elo=1461.5, date="2026-05-01",
                            race_type="Offseason", distance="0",
                        ),
                    ],
                )

            output = root / "output"
            write_site_data(
                men_history=root / "men.csv",
                ladies_history=root / "ladies.csv",
                season=2026,
                output_root=output,
            )

            seasons = json.loads((output / "M/seasons.json").read_text())
            rollover = json.loads((output / "M/2026/race_0.json").read_text())
            self.assertEqual(
                seasons[-1],
                {
                    "season": 2026,
                    "race_count": 1,
                    "winner": {"name": "Winner", "nation": "Norway"},
                },
            )
            self.assertEqual(rollover[0]["previous_elo"], 1400.0)
            self.assertEqual(rollover[0]["new_elo"], 1500.0)
            self.assertEqual(rollover[0]["change"], 100.0)


if __name__ == "__main__":
    unittest.main()

