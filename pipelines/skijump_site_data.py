#!/usr/bin/env python3
"""Build deterministic ski-jumping current Elo and season archive artifacts.

Accepted ski-jumping history stores an offseason row with ``Pelo`` equal to
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
from pathlib import Path
from typing import Any

from pipelines.site_season_snapshot import (
    SeasonSnapshotError,
    build_season_snapshot,
    current_season_rows,
)


CURRENT_RATINGS = {
    "Pelo": "Elo",
    "Small_Pelo": "Small_Elo",
    "Medium_Pelo": "Medium_Elo",
    "Normal_Pelo": "Normal_Elo",
    "Large_Pelo": "Large_Elo",
    "Flying_Pelo": "Flying_Elo",
}

REQUIRED_COLUMNS = {
    "Date", "City", "Country", "Sex", "HillSize", "RaceType", "TeamEvent",
    "Event", "Place", "Skier", "Nation", "ID", "Season", "Race",
    "Birthday", *CURRENT_RATINGS, *CURRENT_RATINGS.values(),
}


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
    return rows


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
                "HillSize": "0",
                "RaceType": "Offseason",
                "TeamEvent": "0",
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
                row["RaceType"], row["HillSize"], row["TeamEvent"],
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
                "hillsize": first["HillSize"],
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


def write_site_data(
    *, men_history: Path, ladies_history: Path, season: int, output_root: Path
) -> None:
    histories = {
        "M": _read_history(men_history, "M"),
        "L": _read_history(ladies_history, "L"),
    }
    output_root = output_root.expanduser().resolve()
    for sex, rows in histories.items():
        _atomic_text(
            output_root / f"{sex}_chrono_current.json",
            json.dumps(
                build_current_table(rows, season),
                ensure_ascii=True,
                separators=(",", ":"),
                allow_nan=False,
            ),
        )
        summaries, races = build_season_archive(rows, season)
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
