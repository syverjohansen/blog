#!/usr/bin/env python3
"""Build deterministic Biathlon current Elo and season archive artifacts.

Accepted Biathlon history stores an offseason row with ``Pelo`` equal to
the undiscounted season-ending rating and ``Elo`` equal to the discounted
rating used to start the next season.  The public Race 0 archive is a season
comparison instead: prior undiscounted close versus the latest actual rating
for athletes who have raced in the requested season.
"""

from __future__ import annotations

import argparse
import csv
import json
import tempfile
from collections import defaultdict
from collections.abc import Iterable
from datetime import date
from pathlib import Path
from typing import Any

from pipelines.site_all_time import build_all_time_elo
from pipelines.site_athletes import (
    COUNTRY_CODES,
    build_athlete_index,
    build_grouped_athlete_histories,
    build_skier_info,
    build_skier_lookup,
)
from pipelines.site_head_to_head import build_head_to_head
from pipelines.site_rankings import build_rankings, read_csv, write_rankings
from pipelines.site_season_snapshot import (
    SeasonSnapshotError,
    build_season_snapshot,
    current_season_rows,
)


CURRENT_RATINGS = {
    "Pelo": "Elo",
    "Individual_Pelo": "Individual_Elo",
    "Pursuit_Pelo": "Pursuit_Elo",
    "Sprint_Pelo": "Sprint_Elo",
    "MassStart_Pelo": "MassStart_Elo",
}

ALL_TIME_RATINGS = {
    "Elo": "Overall",
    "Individual_Elo": "Individual",
    "Pursuit_Elo": "Pursuit",
    "Sprint_Elo": "Sprint",
    "MassStart_Elo": "Mass Start",
}

HEAD_TO_HEAD_FIELDS = {"RaceType": "event"}
ELO_COLUMNS = list(CURRENT_RATINGS.values())
INTEGER_COLUMNS = {"MassStart", "Place", "ID", "Season", "Race", "Exp", "Leg"}
FLOAT_COLUMNS = {"Age", *ELO_COLUMNS, *CURRENT_RATINGS.keys()}

REQUIRED_COLUMNS = {
    "Date", "City", "Country", "Sex", "Distance", "RaceType", "MassStart",
    "Event", "Place", "Skier", "Nation", "ID", "Season", "Race",
    "Birthday", *CURRENT_RATINGS, *CURRENT_RATINGS.values(),
}

# The accepted 2026 compatibility rows for Darya Dolidovich carry a displaced
# event label in Nation. Official IBU evidence identifies her current team as
# the Biathlon Refugee Team; keep this explicit until the historical source row
# is repaired rather than silently presenting the event label as a country.
NATION_OVERRIDES = {"3253": "Biathlon Refugee Team"}


class SiteDataError(RuntimeError):
    """Raised when accepted history cannot produce safe site artifacts."""


def _atomic_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=path.parent, delete=False
    ) as handle:
        handle.write(text)
        temporary = Path(handle.name)
    temporary.replace(path)


def _read_history(path: Path, sex: str) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        rows = list(reader)
        missing = sorted(REQUIRED_COLUMNS - set(reader.fieldnames or []))
    if missing:
        raise SiteDataError(f"{path} is missing columns: {missing}")
    if not rows:
        raise SiteDataError(f"history is empty: {path}")
    if any(row["Sex"] != sex for row in rows):
        raise SiteDataError(f"{path} contains rows outside sex {sex}")
    # A small number of legacy Biathlon rows contain an event label in Nation.
    # Identity metadata is stable across an athlete's accepted history, so use
    # their latest supported nation for site presentation without changing the
    # accepted source file or any race/rating value.
    valid_nation: dict[str, str] = {}
    for row in rows:
        nation = NATION_OVERRIDES.get(row["ID"], row["Nation"].strip())
        if nation in COUNTRY_CODES:
            valid_nation[row["ID"]] = nation
    normalized: list[dict[str, str]] = []
    for row in rows:
        source_nation = row["Nation"].strip()
        nation = NATION_OVERRIDES.get(row["ID"], source_nation)
        if nation not in COUNTRY_CODES:
            try:
                nation = valid_nation[row["ID"]]
            except KeyError as exc:
                raise SiteDataError(
                    f"athlete {row['ID']} has no supported nation identity"
                ) from exc
        if nation != source_nation:
            row = {**row, "Nation": nation}
        normalized.append(row)
    return normalized


def _number(value: str) -> float | None:
    value = value.strip()
    return float(value) if value else None


def _birthday(value: str) -> str | None:
    value = value.strip()
    if not value or value.startswith("1700-01-01"):
        return None
    return value.split("T", 1)[0]


def build_current_table(
    rows: list[dict[str, str]], season: int
) -> list[dict[str, Any]]:
    """Publish each requested-season participant's latest actual Elo."""
    try:
        latest = current_season_rows(rows, season)
    except SeasonSnapshotError as exc:
        raise SiteDataError(str(exc)) from exc

    ordered = sorted(
        latest.values(),
        key=lambda row: (-float(row["Elo"]), row["Skier"], int(row["ID"])),
    )
    output: list[dict[str, Any]] = []
    for place, row in enumerate(ordered, start=1):
        record: dict[str, Any] = {
            "Place": place,
            "Skier": row["Skier"],
            "Nation": row["Nation"],
            "Birthday": _birthday(row["Birthday"]),
        }
        record.update(
            {
                published: _number(row[current])
                for published, current in CURRENT_RATINGS.items()
            }
        )
        record["ID"] = int(row["ID"])
        output.append(record)
    return output


def _has_active_racers(rows: list[dict[str, str]], season: int) -> bool:
    return any(
        int(row["Season"]) == season and row["Event"] != "Offseason"
        for row in rows
    )


def _podium(rows: list[dict[str, str]]) -> list[dict[str, str] | None]:
    by_place: dict[int, dict[str, str]] = {}
    for row in rows:
        by_place.setdefault(int(row["Place"]), row)
    return [
        {"name": by_place[place]["Skier"], "nation": by_place[place]["Nation"]}
        if place in by_place else None
        for place in range(1, 4)
    ]


def build_season_archive(
    rows: list[dict[str, str]], season: int
) -> tuple[list[dict[str, Any]], dict[int, list[dict[str, Any]]]]:
    """Build Race 0 and every accepted race for one season."""
    grouped: dict[int, list[dict[str, str]]] = defaultdict(list)
    try:
        grouped[0] = build_season_snapshot(
            rows,
            season,
            {"Pelo": "Elo"},
            extra_metadata={
                "Distance": "0",
                "RaceType": "Offseason",
                "MassStart": "0",
            },
        )
    except SeasonSnapshotError as exc:
        raise SiteDataError(str(exc)) from exc

    for row in rows:
        if int(row["Season"]) == season and row["Event"] != "Offseason":
            grouped[int(row["Race"])].append(row)
    if len(grouped) == 1:
        raise SiteDataError(f"season {season} contains no race results")

    summaries: list[dict[str, Any]] = []
    races: dict[int, list[dict[str, Any]]] = {}
    for race_number in sorted(grouped):
        if race_number == 0:
            race_rows = sorted(
                grouped[race_number], key=lambda row: -float(row["Elo"])
            )
            for place, row in enumerate(race_rows, start=1):
                row["Place"] = str(place)
        else:
            race_rows = sorted(
                grouped[race_number], key=lambda row: int(row["Place"])
            )

        first = race_rows[0]
        signatures = {
            (
                row["Date"], row["City"], row["Country"], row["Event"],
                row["RaceType"], row["Distance"], row["MassStart"],
            )
            for row in race_rows
        }
        if len(signatures) != 1:
            raise SiteDataError(
                f"season {season} race {race_number} has inconsistent metadata"
            )
        summaries.append(
            {
                "race_number": race_number,
                "date": first["Date"],
                "city": first["City"],
                "country": first["Country"],
                "event": first["Event"],
                "racetype": first["RaceType"],
                "podium": _podium(race_rows),
            }
        )
        races[race_number] = [
            {
                "place": int(row["Place"]),
                "skier": row["Skier"],
                "nation": row["Nation"],
                "id": int(row["ID"]),
                "previous_elo": float(row["Pelo"]),
                "new_elo": float(row["Elo"]),
                "change": float(row["Elo"]) - float(row["Pelo"]),
            }
            for row in race_rows
        ]
    return summaries, races


def _season_winner(
    rows: Iterable[dict[str, str]], season: int
) -> dict[str, str] | None:
    offseason = [
        row
        for row in rows
        if int(row["Season"]) == season and row["Event"] == "Offseason"
    ]
    if not offseason:
        return None
    winner = max(offseason, key=lambda row: float(row["Pelo"]))
    return {"name": winner["Skier"], "nation": winner["Nation"]}


def build_seasons_index(rows: list[dict[str, str]]) -> list[dict[str, Any]]:
    seasons = sorted({int(row["Season"]) for row in rows})
    return [
        {
            "season": season,
            "race_count": len(
                {
                    int(row["Race"])
                    for row in rows
                    if int(row["Season"]) == season
                    and row["Event"] != "Offseason"
                }
            ),
            "winner": _season_winner(rows, season),
        }
        for season in seasons
    ]


def _typed_value(column: str, value: str) -> Any:
    value = value.strip()
    if not value:
        return None
    if column in INTEGER_COLUMNS:
        return int(float(value))
    if column in FLOAT_COLUMNS:
        return float(value)
    return value


def build_athlete_history_updates(
    rows: list[dict[str, str]], season: int
) -> dict[str, dict[str, list[Any]]]:
    """Build the prior rollover plus requested-season profile overlay."""
    prior_offseason = [
        row
        for row in rows
        if row["Event"] == "Offseason" and int(row["Season"]) < season
    ]
    if not prior_offseason:
        raise SiteDataError(f"no offseason ratings exist before season {season}")
    rollover_season = max(int(row["Season"]) for row in prior_offseason)
    selected = [
        row
        for row in rows
        if (
            row["Event"] == "Offseason"
            and int(row["Season"]) == rollover_season
        )
        or int(row["Season"]) == season
    ]

    maxima: dict[tuple[int, int, str], float] = {}
    for row in rows:
        race_key = (int(row["Season"]), int(row["Race"]))
        for column in ELO_COLUMNS:
            value = _number(row[column])
            if value is not None:
                key = (*race_key, column)
                maxima[key] = max(maxima.get(key, value), value)

    by_athlete: dict[str, list[dict[str, Any]]] = defaultdict(list)
    for row in selected:
        record = {
            column: _typed_value(column, value) for column, value in row.items()
        }
        race_key = (int(row["Season"]), int(row["Race"]))
        for column in ELO_COLUMNS:
            value = _number(row[column])
            maximum = maxima.get((*race_key, column))
            record[f"{column}_Pct"] = (
                value / maximum * 100 if value is not None and maximum else 0
            )
        by_athlete[row["ID"]].append(record)

    output: dict[str, dict[str, list[Any]]] = {}
    for athlete_id, athlete_rows in by_athlete.items():
        athlete_rows.sort(key=lambda row: (row["Date"], row["Season"], row["Race"]))
        columns = list(athlete_rows[0])
        output[athlete_id] = {
            column: [row[column] for row in athlete_rows] for column in columns
        }
    return output


def _current_standings(
    rows: list[dict[str, str]], season: int, as_of: date
) -> list[dict[str, str]]:
    """Return accepted target-season standings only after their dated close."""
    output: list[dict[str, str]] = []
    for row in rows:
        if row["Event"] != "Standings" or int(row["Season"]) != season:
            continue
        try:
            standing_date = date.fromisoformat(row["Date"].split("T", 1)[0])
        except ValueError as exc:
            raise SiteDataError(
                f"invalid target-season standings date: {row['Date']!r}"
            ) from exc
        if standing_date > as_of:
            continue
        output.append(
            {
                "ID": row["ID"],
                "Skier": row["Skier"],
                "Nation": row["Nation"],
                "Ranking": row["Place"],
                "Season": row["Season"],
            }
        )
    return output


def write_site_data(
    *,
    men_history: Path,
    ladies_history: Path,
    season: int,
    output_root: Path,
    as_of: date | None = None,
    men_ranking_history: Path | None = None,
    ladies_ranking_history: Path | None = None,
) -> None:
    histories = {
        "M": _read_history(men_history, "M"),
        "L": _read_history(ladies_history, "L"),
    }
    output_root = output_root.expanduser().resolve()
    ranking_histories = {
        "M": men_ranking_history,
        "L": ladies_ranking_history,
    }
    active_sexes = {
        sex: _has_active_racers(rows, season) for sex, rows in histories.items()
    }
    if not any(active_sexes.values()):
        raise SiteDataError(f"season {season} contains no active racers")
    for sex, rows in histories.items():
        _atomic_text(
            output_root / f"{sex}_chrono_all_time.json",
            json.dumps(
                build_all_time_elo(rows, ALL_TIME_RATINGS),
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        _atomic_text(
            output_root / f"{sex}_chrono_current.json",
            json.dumps(
                build_current_table(rows, season) if active_sexes[sex] else [],
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        if active_sexes[sex]:
            summaries, races = build_season_archive(rows, season)
        else:
            # A season can begin with one sex racing before the other. Keep the
            # required per-sex artifacts present and explicitly empty until an
            # accepted result exists; never fill them with prior-season racers.
            summaries, races = [], {0: []}
        season_root = output_root / sex / str(season)
        _atomic_text(
            season_root / "summary.json",
            json.dumps(summaries, ensure_ascii=True, allow_nan=False),
        )
        for race_number, results in races.items():
            _atomic_text(
                season_root / f"race_{race_number}.json",
                json.dumps(results, ensure_ascii=True, allow_nan=False),
            )
        _atomic_text(
            output_root / sex / "seasons.json",
            json.dumps(build_seasons_index(rows), ensure_ascii=True),
        )
        _atomic_text(
            output_root / f"{sex}_athlete_history_updates.json",
            json.dumps(
                build_athlete_history_updates(rows, season),
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        _atomic_text(
            output_root / f"{sex}_all_ids.json",
            json.dumps(
                build_athlete_index(rows),
                ensure_ascii=True,
                separators=(",", ":"),
            ),
        )
        _atomic_text(
            output_root / f"{sex}_skiers_lookup.json",
            json.dumps(
                build_skier_lookup(rows),
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        history_groups = build_grouped_athlete_histories(
            rows,
            elo_columns=ELO_COLUMNS,
            integer_columns=INTEGER_COLUMNS,
            float_columns=FLOAT_COLUMNS,
        )
        for group, histories_by_id in history_groups.items():
            _atomic_text(
                output_root / sex / f"skiers_{group}.json",
                json.dumps(
                    histories_by_id,
                    ensure_ascii=True,
                    separators=(",", ":"),
                    allow_nan=False,
                ),
            )
        head_to_head, race_lookup = build_head_to_head(
            rows, race_fields=HEAD_TO_HEAD_FIELDS
        )
        head_to_head_root = output_root / "head2head"
        _atomic_text(
            head_to_head_root / f"{sex}_head2head_data.json",
            json.dumps(
                head_to_head,
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        _atomic_text(
            head_to_head_root / f"{sex}_race_lookup.json",
            json.dumps(
                race_lookup,
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        _atomic_text(
            output_root / sex / "skier_info.json",
            json.dumps(
                build_skier_info(rows, sex),
                ensure_ascii=True,
                separators=(",", ":"),
            ),
        )
        if ranking_histories[sex] is not None:
            historical = read_csv(
                ranking_histories[sex],
                {
                    "Date", "City", "Event", "RaceType", "MassStart",
                    "Season", "Place", "ID", "Skier", "Nation", "Birthday",
                },
            )
            effective_as_of = as_of or date.today()
            write_rankings(
                output_root,
                sex,
                build_rankings(
                    sport="biathlon",
                    history_rows=rows,
                    historical_ranking_rows=historical,
                    target_season=season,
                    as_of=effective_as_of,
                    current_standings_rows=_current_standings(
                        historical, season, effective_as_of
                    ),
                ),
            )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--men-history", required=True, type=Path)
    parser.add_argument("--ladies-history", required=True, type=Path)
    parser.add_argument("--season", required=True, type=int)
    parser.add_argument("--output-root", required=True, type=Path)
    parser.add_argument("--as-of", required=True, type=date.fromisoformat)
    parser.add_argument("--men-ranking-history", required=True, type=Path)
    parser.add_argument("--ladies-ranking-history", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    write_site_data(
        men_history=args.men_history,
        ladies_history=args.ladies_history,
        season=args.season,
        output_root=args.output_root,
        as_of=args.as_of,
        men_ranking_history=args.men_ranking_history,
        ladies_ranking_history=args.ladies_ranking_history,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

