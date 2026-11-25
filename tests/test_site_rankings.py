import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.site_rankings import RankingDataError, build_rankings, write_rankings


def race(
    *, athlete_id: int, name: str, event: str, place: int, season: int,
    distance: str = "10", birthday: str = "2000-11-19T00:00:00",
    race_type: str = "Large", team_event: str = "0", city: str = "Oslo",
) -> dict[str, str]:
    return {
        "ID": str(athlete_id), "Skier": name, "Nation": "NOR",
        "Birthday": birthday, "Event": event, "Place": str(place),
        "Season": str(season), "Distance": distance, "RaceType": race_type,
        "TeamEvent": team_event, "City": city,
    }


def standings(
    *, athlete_id: int, name: str, place: int, season: int
) -> dict[str, str]:
    return {
        **race(
            athlete_id=athlete_id, name=name, event="Standings", place=place,
            season=season, distance="0",
        ),
    }


class SiteRankingsTest(unittest.TestCase):
    def test_alpine_scores_races_and_only_completed_historical_standings(self) -> None:
        rows = [
            race(athlete_id=1, name="Winner", event="Olympic Games", place=1, season=2026),
            race(athlete_id=1, name="Winner", event="WSC", place=2, season=2026),
            race(athlete_id=1, name="Winner", event="World Cup", place=3, season=2027),
            race(athlete_id=2, name="Fourth", event="World Cup", place=4, season=2027),
        ]
        historical = [
            standings(athlete_id=1, name="Winner", place=1, season=2026),
            standings(athlete_id=1, name="Winner", place=1, season=2027),
        ]

        result = build_rankings(
            sport="alpine",
            history_rows=rows,
            historical_ranking_rows=historical,
            target_season=2027,
            as_of=date(2026, 11, 18),
        )

        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]["Olympics"], 80.0)
        self.assertEqual(result[0]["WSC"], 20.0)
        self.assertEqual(result[0]["WC"], 4.0)
        self.assertEqual(result[0]["Table"], 80.0)
        self.assertEqual(result[0]["Total"], 184.0)
        self.assertEqual(result[0]["From"], 2026)
        self.assertEqual(result[0]["To"], 2027)
        self.assertEqual(result[0]["Age"], 25)

    def test_official_standings_are_added_only_when_explicitly_supplied(self) -> None:
        rows = [race(athlete_id=1, name="Winner", event="World Cup", place=1, season=2027)]
        official = [
            {
                "ID": "1", "Skier": "Winner", "Nation": "NOR",
                "Ranking": "1", "Season": "2027",
            }
        ]
        without = build_rankings(
            sport="alpine", history_rows=rows, historical_ranking_rows=[],
            target_season=2027, as_of=date(2027, 5, 1),
        )
        with_final = build_rankings(
            sport="alpine", history_rows=rows, historical_ranking_rows=[],
            target_season=2027, as_of=date(2027, 5, 1),
            current_standings_rows=official,
        )
        self.assertEqual(without[0]["Table"], 0.0)
        self.assertEqual(with_final[0]["Table"], 80.0)

    def test_non_podium_duplicate_historical_standings_do_not_double_score(self) -> None:
        rows = [race(athlete_id=1, name="Winner", event="World Cup", place=1, season=2027)]
        result = build_rankings(
            sport="alpine", history_rows=rows,
            historical_ranking_rows=[
                standings(athlete_id=1, name="Winner", place=2, season=2026),
                standings(athlete_id=1, name="Winner", place=80, season=2026),
            ],
            target_season=2027, as_of=date(2027, 1, 1),
        )
        self.assertEqual(result[0]["Table"], 40.0)

    def test_cross_country_scores_tour_relay_and_team_sprint_members(self) -> None:
        history = [
            race(athlete_id=1, name="Skier One", event="Tour de Ski", place=1, season=2027),
            race(athlete_id=2, name="Skier Two", event="World Cup", place=2, season=2027),
        ]
        team_rows = [
            {
                "Season": "2027", "Place": "1", "Event": "World Championship",
                "Distance": "Rel", "Sex": "M", "Nation": "NOR",
                "TeamMembers": "Skier One,Skier Two", "MemberIDs": "1,2",
            },
            {
                "Season": "2027", "Place": "2", "Event": "Olympic Games",
                "Distance": "Ts", "Sex": "M", "Nation": "NOR",
                "TeamMembers": "Skier One,Skier Two", "MemberIDs": "1,2",
            },
        ]
        result = build_rankings(
            sport="cross-country", history_rows=history,
            historical_ranking_rows=[], target_season=2027,
            as_of=date(2027, 2, 20), team_rows=team_rows,
        )
        by_id = {row["ID"]: row for row in result}
        self.assertEqual(by_id[1]["Tour"], 20.0)
        self.assertEqual(by_id[1]["WSC"], 10.0)
        self.assertEqual(by_id[1]["Olympics"], 20.0)
        self.assertEqual(by_id[2]["WC"], 4.0)
        self.assertEqual(by_id[2]["WSC"], 10.0)
        self.assertEqual(by_id[2]["Olympics"], 20.0)

    def test_ski_jumping_scores_individual_team_and_tournament_results(self) -> None:
        history = [
            race(
                athlete_id=1, name="Jumper", event="Olympic Games", place=1,
                season=2026, race_type="Team Large", team_event="1",
            ),
            race(
                athlete_id=1, name="Jumper", event="World Championship",
                place=2, season=2026,
            ),
            race(
                athlete_id=1, name="Jumper",
                event="Ski Flying World Championship", place=1, season=2026,
                race_type="Flying",
            ),
            race(
                athlete_id=1, name="Jumper", event="World Cup", place=3,
                season=2027, race_type="Team Large", team_event="1",
            ),
            race(
                athlete_id=1, name="Jumper", event="World Cup", place=1,
                season=2027,
            ),
        ]
        historical = [
            race(
                athlete_id=1, name="Jumper", event="4 Hill Tournament",
                place=1, season=2026, race_type="4 Hill Tournament",
                city="4 Hill Tournament",
            ),
            standings(athlete_id=1, name="Jumper", place=1, season=2026),
            standings(athlete_id=1, name="Jumper", place=1, season=2027),
        ]

        result = build_rankings(
            sport="ski-jumping",
            history_rows=history,
            historical_ranking_rows=historical,
            target_season=2027,
            as_of=date(2026, 11, 18),
        )[0]

        self.assertEqual(result["Olympics"], 20.0)
        self.assertEqual(result["WSC"], 20.0)
        self.assertEqual(result["4 Hills"], 20.0)
        self.assertEqual(result["Ski Flying WC"], 20.0)
        self.assertEqual(result["World Cup"], 9.0)
        self.assertEqual(result["Table"], 80.0)
        self.assertEqual(result["Total"], 169.0)

    def test_rejects_mismatched_team_members(self) -> None:
        with self.assertRaisesRegex(RankingDataError, "mismatched"):
            build_rankings(
                sport="cross-country",
                history_rows=[race(athlete_id=1, name="One", event="World Cup", place=1, season=2027)],
                historical_ranking_rows=[], target_season=2027,
                as_of=date(2027, 1, 1),
                team_rows=[{
                    "Season": "2027", "Place": "1", "Event": "World Cup",
                    "Distance": "Rel", "Sex": "M", "Nation": "NOR",
                    "TeamMembers": "One,Two", "MemberIDs": "1",
                }],
            )

    def test_writes_stable_json_and_csv_contracts(self) -> None:
        rows = build_rankings(
            sport="alpine",
            history_rows=[race(athlete_id=1, name="Winner", event="World Cup", place=1, season=2027)],
            historical_ranking_rows=[], target_season=2027,
            as_of=date(2027, 1, 1),
        )
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            write_rankings(output, "M", rows)
            self.assertEqual(json.loads((output / "M/ranks.json").read_text()), rows)
            self.assertIn("Skier,Nation,ID,Olympics", (output / "M/ranks.csv").read_text())


if __name__ == "__main__":
    unittest.main()
