#!/usr/bin/env python3
"""Build deterministic Nordic Numbers career-ranking artifacts.

Race points come from the accepted Elo histories (and, for cross-country,
the accepted relay/team-sprint histories).  Completed-season table points
come from the historical rankings source.  A freshly validated official FIS
standings file may be supplied explicitly when the current season is closed.
"""

from __future__ import annotations

import csv
import json
import tempfile
from collections import defaultdict
from datetime import date, datetime
from pathlib import Path
from typing import Iterable


class RankingDataError(RuntimeError):
    """Raised when ranking inputs cannot produce a trustworthy artifact."""


PODIUM_POINTS = {
    "standings": (80.0, 40.0, 20.0),
    "alpine_olympics": (80.0, 40.0, 20.0),
    "alpine_wsc": (40.0, 20.0, 10.0),
    "alpine_wc": (8.0, 4.0, 4.0),
    "xc_olympics_individual": (80.0, 40.0, 20.0),
    "xc_olympics_relay": (20.0, 10.0, 5.0),
    "xc_olympics_ts": (40.0, 20.0, 10.0),
    "xc_wsc_individual": (40.0, 20.0, 10.0),
    "xc_wsc_relay": (10.0, 5.0, 2.5),
    "xc_wsc_ts": (20.0, 10.0, 5.0),
    "xc_tour_individual": (20.0, 10.0, 5.0),
    "xc_tour_relay": (20.0, 10.0, 5.0),
    "xc_tour_ts": (20.0, 10.0, 5.0),
    "xc_wc_individual": (8.0, 4.0, 4.0),
    "xc_wc_relay": (2.0, 1.0, 1.0),
    "xc_wc_ts": (4.0, 2.0, 2.0),
}


def read_csv(path: Path, required: set[str]) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        rows = list(reader)
        missing = sorted(required - set(reader.fieldnames or []))
    if missing:
        raise RankingDataError(f"{path} is missing columns: {missing}")
    if not rows:
        raise RankingDataError(f"ranking input is empty: {path}")
    return rows


def read_finalized_standings(root: Path | None, gender: str) -> list[dict[str, str]]:
    """Read immutable, season-keyed official standings snapshots if present."""
    if root is None or not root.exists():
        return []
    rows: list[dict[str, str]] = []
    for season_root in sorted(path for path in root.iterdir() if path.is_dir()):
        try:
            season = int(season_root.name)
        except ValueError as exc:
            raise RankingDataError(
                f"invalid finalized standings season directory: {season_root}"
            ) from exc
        path = season_root / f"{gender}_standings.csv"
        if not path.is_file():
            raise RankingDataError(f"missing finalized standings snapshot: {path}")
        season_rows = read_csv(path, {"Skier", "ID", "Nation", "Ranking"})
        for row in season_rows:
            rows.append({**row, "Season": str(season)})
    return rows


def _integer(row: dict[str, str], field: str, source: str) -> int:
    try:
        return int(float(row[field]))
    except (KeyError, TypeError, ValueError) as exc:
        raise RankingDataError(f"{source}: invalid {field}: {row.get(field)!r}") from exc


def _podium_points(place: int, key: str) -> float:
    return PODIUM_POINTS[key][place - 1] if 1 <= place <= 3 else 0.0


def _xc_race_type(distance: str) -> str:
    if distance == "Rel":
        return "relay"
    if distance == "Ts":
        return "ts"
    return "individual"


def _race_category_and_points(
    row: dict[str, str], *, sport: str
) -> tuple[str, float]:
    place = _integer(row, "Place", "race result")
    if place > 3:
        return "WC", 0.0
    event = row["Event"].strip()
    if event == "Offseason":
        return "WC", 0.0
    olympics = "olympic" in event.lower()
    if sport == "alpine":
        if olympics:
            return "Olympics", _podium_points(place, "alpine_olympics")
        if event == "WSC":
            return "WSC", _podium_points(place, "alpine_wsc")
        return "WC", _podium_points(place, "alpine_wc")

    race_type = _xc_race_type(row["Distance"].strip())
    if olympics:
        return "Olympics", _podium_points(place, f"xc_olympics_{race_type}")
    if event == "World Championship":
        return "WSC", _podium_points(place, f"xc_wsc_{race_type}")
    if event == "Tour de Ski":
        return "Tour", _podium_points(place, f"xc_tour_{race_type}")
    return "WC", _podium_points(place, f"xc_wc_{race_type}")


def _age(birthday: str, as_of: date) -> int | None:
    birthday = birthday.strip().split("T", 1)[0]
    if not birthday or birthday.startswith("1700-01-01"):
        return None
    try:
        born = datetime.strptime(birthday, "%Y-%m-%d").date()
    except ValueError as exc:
        raise RankingDataError(f"invalid Birthday: {birthday!r}") from exc
    return as_of.year - born.year - ((as_of.month, as_of.day) < (born.month, born.day))


def _team_members(row: dict[str, str]) -> list[tuple[str, str]]:
    names = [value.strip() for value in row["TeamMembers"].split(",") if value.strip()]
    ids = [value.strip() for value in row["MemberIDs"].split(",") if value.strip()]
    if not names or len(names) != len(ids):
        raise RankingDataError(
            "team result has mismatched TeamMembers and MemberIDs: "
            f"{row.get('Season')}/{row.get('Race')}/{row.get('Nation')}"
        )
    return list(zip(ids, names, strict=True))


def build_rankings(
    *,
    sport: str,
    history_rows: list[dict[str, str]],
    historical_ranking_rows: list[dict[str, str]],
    target_season: int,
    as_of: date,
    team_rows: Iterable[dict[str, str]] = (),
    current_standings_rows: list[dict[str, str]] | None = None,
) -> list[dict[str, object]]:
    """Return one sex's deterministic career ranking table.

    Historical ranking rows are used only for completed-season ``Standings``
    records.  Race results always come from accepted modern histories.
    """
    if sport not in {"alpine", "cross-country"}:
        raise RankingDataError(f"unsupported ranking sport: {sport}")

    categories = ("Olympics", "WSC", "Tour", "WC", "Table")
    points: dict[str, dict[str, float]] = defaultdict(
        lambda: {category: 0.0 for category in categories}
    )
    seasons: dict[str, set[int]] = defaultdict(set)
    identity: dict[str, dict[str, str]] = {}

    for row in history_rows:
        athlete_id = str(_integer(row, "ID", "accepted history"))
        identity[athlete_id] = row
        season = _integer(row, "Season", "accepted history")
        if row["Event"] != "Offseason":
            seasons[athlete_id].add(season)
        category, score = _race_category_and_points(row, sport=sport)
        points[athlete_id][category] += score

    if sport == "cross-country":
        for row in team_rows:
            season = _integer(row, "Season", "accepted team history")
            category, score = _race_category_and_points(row, sport=sport)
            for athlete_id, name in _team_members(row):
                athlete_id = str(int(athlete_id))
                seasons[athlete_id].add(season)
                points[athlete_id][category] += score
                if athlete_id not in identity:
                    identity[athlete_id] = {
                        "ID": athlete_id,
                        "Skier": name,
                        "Nation": row["Nation"],
                        "Birthday": "",
                    }

    seen_standings: set[tuple[int, str]] = set()
    podium_standings: set[tuple[int, str]] = set()
    for row in historical_ranking_rows:
        athlete_id = str(_integer(row, "ID", "historical ranking history"))
        if athlete_id not in identity or not identity[athlete_id].get("Birthday", "").strip():
            identity[athlete_id] = row
        if row["Event"] != "Standings":
            continue
        season = _integer(row, "Season", "historical standings")
        if season >= target_season:
            continue
        key = (season, athlete_id)
        seen_standings.add(key)
        place = _integer(row, "Place", "historical standings")
        if place <= 3 and key in podium_standings:
            raise RankingDataError(
                f"multiple points-bearing historical standings rows: {key}"
            )
        if place <= 3:
            podium_standings.add(key)
        points[athlete_id]["Table"] += _podium_points(place, "standings")
        seasons[athlete_id].add(season)
        identity.setdefault(athlete_id, row)

    for row in current_standings_rows or []:
        standings_season = _integer(row, "Season", "official finalized standings")
        if standings_season > target_season:
            continue
        athlete_id = str(_integer(row, "ID", "official current standings"))
        key = (standings_season, athlete_id)
        if key in seen_standings:
            raise RankingDataError(f"duplicate current standings row: {key}")
        seen_standings.add(key)
        place = _integer(row, "Ranking", "official current standings")
        points[athlete_id]["Table"] += _podium_points(place, "standings")
        seasons[athlete_id].add(standings_season)
        identity.setdefault(
            athlete_id,
            {
                "ID": athlete_id,
                "Skier": row["Skier"],
                "Nation": row["Nation"],
                "Birthday": "",
            },
        )

    result: list[dict[str, object]] = []
    for athlete_id, category_points in points.items():
        total = sum(category_points.values())
        if not total:
            continue
        row = identity.get(athlete_id)
        if row is None:
            raise RankingDataError(f"ranking athlete {athlete_id} has no identity")
        athlete_seasons = seasons[athlete_id]
        record: dict[str, object] = {
            "Skier": row["Skier"],
            "Nation": row["Nation"],
            "ID": int(athlete_id),
            "Olympics": category_points["Olympics"],
            "WSC": category_points["WSC"],
        }
        if sport == "cross-country":
            record["Tour"] = category_points["Tour"]
        record.update(
            {
                "WC": category_points["WC"],
                "Table": category_points["Table"],
                "Total": total,
                "From": min(athlete_seasons),
                "To": max(athlete_seasons),
                "Age": _age(row.get("Birthday", ""), as_of),
            }
        )
        result.append(record)
    return sorted(result, key=lambda row: (-float(row["Total"]), row["Skier"], row["ID"]))


def write_rankings(output_root: Path, sex: str, rows: list[dict[str, object]]) -> None:
    if not rows:
        raise RankingDataError(f"ranking output for {sex} is empty")
    destination = output_root / sex
    destination.mkdir(parents=True, exist_ok=True)
    json_path = destination / "ranks.json"
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", dir=destination, delete=False
    ) as handle:
        json.dump(rows, handle, ensure_ascii=True, separators=(",", ":"), allow_nan=False)
        json_temporary = Path(handle.name)
    json_temporary.replace(json_path)

    csv_path = destination / "ranks.csv"
    with tempfile.NamedTemporaryFile(
        "w", encoding="utf-8", newline="", dir=destination, delete=False
    ) as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
        csv_temporary = Path(handle.name)
    csv_temporary.replace(csv_path)
