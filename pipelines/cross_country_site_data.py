#!/usr/bin/env python3
"""Build current cross-country Elo tables and one season's race archive."""

from __future__ import annotations

import argparse
import csv
import json
import tempfile
from collections import defaultdict
from collections.abc import Iterable
from pathlib import Path
from typing import Any

from pipelines.site_athletes import build_athlete_index, build_skier_info

CURRENT_RATINGS = {
    "Pelo": "Elo",
    "Distance_Pelo": "Distance_Elo",
    "Distance_C_Pelo": "Distance_C_Elo",
    "Distance_F_Pelo": "Distance_F_Elo",
    "Sprint_Pelo": "Sprint_Elo",
    "Sprint_C_Pelo": "Sprint_C_Elo",
    "Sprint_F_Pelo": "Sprint_F_Elo",
    "Classic_Pelo": "Classic_Elo",
    "Freestyle_Pelo": "Freestyle_Elo",
}

ELO_COLUMNS = list(CURRENT_RATINGS.values())
INTEGER_COLUMNS = {"MS", "Place", "ID", "Season", "Race", "Exp"}
FLOAT_COLUMNS = {"Age", *ELO_COLUMNS, *CURRENT_RATINGS.keys()}


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
        fields = set(reader.fieldnames or [])
    required = {
        "Date",
        "City",
        "Country",
        "Sex",
        "Distance",
        "Technique",
        "Event",
        "Place",
        "Skier",
        "Nation",
        "ID",
        "Season",
        "Race",
        "Birthday",
        *CURRENT_RATINGS.keys(),
        *CURRENT_RATINGS.values(),
    }
    missing = sorted(required - fields)
    if missing:
        raise SiteDataError(f"{path} is missing columns: {missing}")
    if not rows:
        raise SiteDataError(f"history is empty: {path}")
    if any(row["Sex"] != sex for row in rows):
        raise SiteDataError(f"{path} contains rows outside sex {sex}")
    return rows


def _number(value: str) -> float | None:
    value = value.strip()
    return float(value) if value else None


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
    """Build the prior offseason plus current-season profile overlay."""
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


def _birthday(value: str) -> str | None:
    value = value.strip()
    if not value or value.startswith("1700-01-01"):
        return None
    return value.split("T", 1)[0]


def build_current_table(
    rows: list[dict[str, str]], season: int
) -> list[dict[str, Any]]:
    active_ids = {
        row["ID"]
        for row in rows
        if int(row["Season"]) == season and row["Event"] != "Offseason"
    }
    if not active_ids:
        raise SiteDataError(f"season {season} contains no active racers")

    latest: dict[str, dict[str, str]] = {}
    for row in rows:
        if row["ID"] in active_ids and int(row["Season"]) <= season:
            latest[row["ID"]] = row
    if set(latest) != active_ids:
        raise SiteDataError("not every active athlete has a latest history row")

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
                published: _number(row[source])
                for published, source in CURRENT_RATINGS.items()
            }
        )
        record["ID"] = int(row["ID"])
        output.append(record)
    return output


def _podium(rows: list[dict[str, str]]) -> list[dict[str, str] | None]:
    by_place = {int(row["Place"]): row for row in rows}
    return [
        {"name": by_place[place]["Skier"], "nation": by_place[place]["Nation"]}
        if place in by_place
        else None
        for place in range(1, 4)
    ]


def build_season_archive(
    rows: list[dict[str, str]], season: int
) -> tuple[list[dict[str, Any]], dict[int, list[dict[str, Any]]]]:
    grouped: dict[int, list[dict[str, str]]] = defaultdict(list)
    prior_offseason = [
        row
        for row in rows
        if row["Event"] == "Offseason" and int(row["Season"]) < season
    ]
    if not prior_offseason:
        raise SiteDataError(f"no offseason ratings exist before season {season}")
    rollover_season = max(int(row["Season"]) for row in prior_offseason)
    active_ids = {
        row["ID"]
        for row in rows
        if int(row["Season"]) == season and row["Event"] != "Offseason"
    }
    rollover_rows = [
        row.copy()
        for row in prior_offseason
        if int(row["Season"]) == rollover_season and row["ID"] in active_ids
    ]
    rollover_ids = {row["ID"] for row in rollover_rows}
    rollover_template = next(
        row for row in prior_offseason if int(row["Season"]) == rollover_season
    )
    for athlete_id in active_ids - rollover_ids:
        first_start = next(
            row
            for row in rows
            if row["ID"] == athlete_id
            and int(row["Season"]) == season
            and row["Event"] != "Offseason"
        )
        starting_row = first_start.copy()
        for column in ("Date", "City", "Country", "Event", "Distance", "Technique"):
            starting_row[column] = rollover_template[column]
        for previous, current in CURRENT_RATINGS.items():
            starting_row[previous] = first_start[previous]
            starting_row[current] = first_start[previous]
        rollover_rows.append(starting_row)
    grouped[0] = rollover_rows
    for row in rows:
        if int(row["Season"]) == season and row["Event"] != "Offseason":
            grouped[int(row["Race"])].append(row)
    if len(grouped) == 1:
        raise SiteDataError(f"season {season} contains no race results")

    summaries: list[dict[str, Any]] = []
    races: dict[int, list[dict[str, Any]]] = {}
    for race_number in sorted(grouped):
        if race_number == 0:
            race_rows = sorted(grouped[race_number], key=lambda row: -float(row["Elo"]))
            for place, row in enumerate(race_rows, start=1):
                row["Place"] = str(place)
        else:
            race_rows = sorted(grouped[race_number], key=lambda row: int(row["Place"]))
        first = race_rows[0]
        signatures = {
            (
                row["Date"],
                row["City"],
                row["Country"],
                row["Event"],
                row["Distance"],
                row["Technique"],
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
                "distance": first["Distance"],
                "technique": first["Technique"],
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
    winner = max(offseason, key=lambda row: float(row["Elo"]))
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
                    if int(row["Season"]) == season and row["Event"] != "Offseason"
                }
            ),
            "winner": _season_winner(rows, season),
        }
        for season in seasons
    ]


def write_site_data(
    *, men_history: Path, ladies_history: Path, season: int, output_root: Path
) -> None:
    histories = {
        "M": _read_history(men_history, "M"),
        "L": _read_history(ladies_history, "L"),
    }
    output_root = output_root.expanduser().resolve()
    for sex, rows in histories.items():
        current = build_current_table(rows, season)
        summaries, races = build_season_archive(rows, season)
        _atomic_text(
            output_root / f"{sex}_chrono_current.json",
            json.dumps(current, ensure_ascii=True, separators=(",", ":")),
        )
        season_root = output_root / sex / str(season)
        _atomic_text(
            season_root / "summary.json",
            json.dumps(summaries, ensure_ascii=True),
        )
        for race_number, results in races.items():
            _atomic_text(
                season_root / f"race_{race_number}.json",
                json.dumps(results, ensure_ascii=True),
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
            output_root / sex / "skier_info.json",
            json.dumps(
                build_skier_info(rows, sex),
                ensure_ascii=True,
                separators=(",", ":"),
            ),
        )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--men-history", required=True, type=Path)
    parser.add_argument("--ladies-history", required=True, type=Path)
    parser.add_argument("--season", required=True, type=int)
    parser.add_argument("--output-root", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    write_site_data(
        men_history=args.men_history,
        ladies_history=args.ladies_history,
        season=args.season,
        output_root=args.output_root,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
