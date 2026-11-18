"""Build deterministic head-to-head race histories from accepted Elo data."""

from __future__ import annotations

from collections import defaultdict
from datetime import datetime
import math
from typing import Any


class HeadToHeadError(RuntimeError):
    """Raised when accepted history cannot produce safe matchup data."""


def _integer(value: str, field: str) -> int:
    try:
        return int(float(value))
    except (TypeError, ValueError) as error:
        raise HeadToHeadError(f"invalid {field}: {value!r}") from error


def _number(value: str) -> int | float | None:
    value = value.strip()
    if not value or value.lower() in {"none", "nan", "inf", "-inf", "infinity"}:
        return None
    try:
        number = float(value)
    except ValueError as error:
        raise HeadToHeadError(f"invalid numeric value: {value!r}") from error
    if not math.isfinite(number):
        return None
    return int(number) if number.is_integer() else number


def _date(value: str) -> str:
    value = value.strip()
    if not value:
        return "Unknown"
    try:
        return datetime.fromisoformat(value).date().isoformat()
    except ValueError:
        return "Unknown"


def _text(value: str, *, default: str = "Unknown") -> str:
    value = value.strip()
    return value or default


def build_head_to_head(
    rows: list[dict[str, str]],
    *,
    race_fields: dict[str, str],
    athlete_defaults: dict[str, str] | None = None,
    lookup_field_names: set[str] | None = None,
) -> tuple[dict[str, list[dict[str, Any]]], dict[str, dict[str, Any]]]:
    """Return athlete race histories and a validated season/race lookup."""
    if not rows:
        raise HeadToHeadError("athlete history is empty")
    defaults = athlete_defaults or {}
    lookup_names = lookup_field_names or set(race_fields.values())
    required = {
        "ID", "Skier", "Season", "Race", "Date", "City", "Country",
        "Place", "Elo", *race_fields,
    }
    missing = sorted(required - set(rows[0]))
    if missing:
        raise HeadToHeadError(f"athlete history is missing columns: {missing}")

    normalized: list[tuple[tuple[Any, ...], str, dict[str, Any], dict[str, Any]]] = []
    signatures: dict[str, dict[str, Any]] = {}
    for index, row in enumerate(rows):
        race = _integer(row["Race"], "race")
        if race == 0:
            continue
        season = _integer(row["Season"], "season")
        athlete_id = _integer(row["ID"], "athlete ID")
        if athlete_id <= 0:
            raise HeadToHeadError(f"invalid athlete ID: {athlete_id}")
        date = _date(row["Date"])
        race_values = {
            published: _text(row[source], default=defaults.get(source, "Unknown"))
            for source, published in race_fields.items()
        }
        race_signature: dict[str, Any] = {
            "season": season,
            "race": race,
            "date": date,
            "city": _text(row["City"]),
            "country": _text(row["Country"]),
            **race_values,
        }
        lookup_key = f"{season}_{race}"
        prior = signatures.setdefault(lookup_key, race_signature)
        if prior != race_signature:
            raise HeadToHeadError(
                f"race {lookup_key} has conflicting metadata: {prior!r} != {race_signature!r}"
            )

        race_metadata = {
            key: value
            for key, value in race_signature.items()
            if key in {"season", "race", "date", "city", "country"} or key in lookup_names
        }

        athlete_race = {
            "season": str(season),
            "race": str(race),
            "date": date,
            "city": race_signature["city"],
            "country": race_signature["country"],
            **race_values,
            "place": _number(row["Place"]),
            "elo": _number(row["Elo"]),
            "skier_name": row["Skier"].strip() or None,
        }
        sort_date = date if date != "Unknown" else "9999-12-31"
        normalized.append(
            ((sort_date, season, race, index), str(athlete_id), athlete_race, race_metadata)
        )

    if not normalized:
        raise HeadToHeadError("athlete history contains no races")
    normalized.sort(key=lambda item: item[0])

    by_athlete: dict[str, list[dict[str, Any]]] = defaultdict(list)
    race_lookup: dict[str, dict[str, Any]] = {}
    for _, athlete_id, athlete_race, race_metadata in normalized:
        by_athlete[athlete_id].append(athlete_race)
        race_lookup[f"{race_metadata['season']}_{race_metadata['race']}"] = race_metadata
    return (
        {athlete_id: by_athlete[athlete_id] for athlete_id in sorted(by_athlete)},
        race_lookup,
    )
