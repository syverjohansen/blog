"""Build a read-only daily Nordic Combined prediction execution contract.

This module plans work only. It never scrapes, simulates, publishes, or writes
inside either source repository. Later hosted slices consume this plan instead
of rediscovering event and artifact semantics in shell code.
"""

from __future__ import annotations

import argparse
import csv
from dataclasses import asdict, dataclass
from datetime import date, datetime
import json
from pathlib import Path
from typing import Iterable


SCHEMA_VERSION = "nordicnumbers.nordic-combined-prediction-plan/v1"
SCHEDULE_COLUMNS = {
    "Date", "Startlist", "Sex", "City", "Country", "Distance",
    "RaceType", "Championship", "Race",
}

CATEGORY_CONTRACTS = {
    "men": {
        "workbook": "men_position_probabilities.xlsx",
        "sheet_prefix": "Men Race",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/excel365/startlist_races_men.csv",
        ),
    },
    "ladies": {
        "workbook": "ladies_position_probabilities.xlsx",
        "sheet_prefix": "Ladies Race",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/excel365/startlist_races_ladies.csv",
        ),
    },
    "men_team": {
        "workbook": "men_team_position_probabilities.xlsx",
        "sheet_prefix": "Men Team",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/men_team_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_team_races_men.csv",
        ),
    },
    "ladies_team": {
        "workbook": "ladies_team_position_probabilities.xlsx",
        "sheet_prefix": "Ladies Team",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/ladies_team_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_team_races_ladies.csv",
        ),
    },
    "men_team_sprint": {
        "workbook": "men_team_sprint_position_probabilities.xlsx",
        "sheet_prefix": "Men Team Sprint",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/team_sprint_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_team_sprint_races_men.csv",
        ),
    },
    "ladies_team_sprint": {
        "workbook": "ladies_team_sprint_position_probabilities.xlsx",
        "sheet_prefix": "Ladies Team Sprint",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/team_sprint_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_team_sprint_races_ladies.csv",
        ),
    },
    "mixed_team": {
        "workbook": "mixed_team_position_probabilities.xlsx",
        "sheet_prefix": "Mixed Team",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/mixed_team_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_mixed_team_races_teams.csv",
        ),
    },
    "mixed_team_sprint": {
        "workbook": "mixed_team_sprint_position_probabilities.xlsx",
        "sheet_prefix": "Mixed Team Sprint",
        "inputs": (
            "elo/python/nordic-combined/polars/excel365/men_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/excel365/ladies_chrono_elevation.csv",
            "elo/python/nordic-combined/polars/relay/excel365/team_sprint_chrono.csv",
            "elo/python/nordic-combined/polars/relay/excel365/startlist_mixed_team_sprint_races_teams.csv",
        ),
    },
}


class PredictionContractError(ValueError):
    """Raised when accepted schedule data cannot form an unambiguous plan."""


@dataclass(frozen=True)
class PredictionEvent:
    category: str
    sex: str
    race_type: str
    city: str
    country: str
    distance: str
    race: int
    startlist: str


def _parse_schedule_date(value: str) -> date:
    value = value.strip()
    for pattern in ("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value, pattern).date()
        except ValueError:
            pass
    raise PredictionContractError(f"unsupported schedule date: {value!r}")


def _category(row: dict[str, str]) -> str:
    sex = row["Sex"].strip().casefold()
    race_type = row["RaceType"].strip().casefold()
    is_team_sprint = "team sprint" in race_type
    is_team = "team" in race_type
    if sex == "mixed":
        if not is_team:
            raise PredictionContractError("mixed-sex schedule row is not a team event")
        return "mixed_team_sprint" if is_team_sprint else "mixed_team"
    if sex not in {"m", "l"}:
        raise PredictionContractError(f"unsupported Nordic Combined sex: {row['Sex']!r}")
    if is_team:
        label = "men" if sex == "m" else "ladies"
        suffix = "team_sprint" if is_team_sprint else "team"
        return f"{label}_{suffix}"
    return "men" if sex == "m" else "ladies"


def read_events(schedule: Path, logical_date: date) -> tuple[PredictionEvent, ...]:
    with schedule.open(newline="", encoding="utf-8-sig") as handle:
        reader = csv.DictReader(handle)
        missing = sorted(SCHEDULE_COLUMNS - set(reader.fieldnames or ()))
        if missing:
            raise PredictionContractError(
                f"schedule is missing required columns: {missing}"
            )
        selected = [
            row for row in reader
            if _parse_schedule_date(row["Date"]) == logical_date
        ]

    events: list[PredictionEvent] = []
    identities: set[tuple[str, int]] = set()
    for row in selected:
        try:
            race = int(row["Race"])
        except ValueError as error:
            raise PredictionContractError(
                f"race must be an integer: {row['Race']!r}"
            ) from error
        event = PredictionEvent(
            category=_category(row),
            sex=row["Sex"].strip(),
            race_type=row["RaceType"].strip(),
            city=row["City"].strip(),
            country=row["Country"].strip(),
            distance=row["Distance"].strip(),
            race=race,
            startlist=row["Startlist"].strip(),
        )
        identity = (event.sex, event.race)
        if identity in identities:
            raise PredictionContractError(f"duplicate schedule identity: {identity}")
        identities.add(identity)
        events.append(event)
    return tuple(events)


def _json_outputs(category: str, count: int) -> list[str]:
    contract = CATEGORY_CONTRACTS[category]
    base = Path(str(contract["workbook"])).stem
    if count == 1:
        return [f"{base}.json"]
    prefix = str(contract["sheet_prefix"])
    safe_prefix = "".join(character if character.isalnum() else "_" for character in prefix)
    return [f"{base}_{safe_prefix}_{index}.json" for index in range(1, count + 1)]


def build_plan(schedule: Path, logical_date: date) -> dict[str, object]:
    events = read_events(schedule, logical_date)
    date_key = logical_date.strftime("%Y%m%d")
    category_counts = {
        category: sum(event.category == category for event in events)
        for category in CATEGORY_CONTRACTS
    }
    blockers = [
        (
            f"{category} has {count} events; the legacy model retains only one "
            "same-category team result"
        )
        for category, count in category_counts.items()
        if "team" in category and count > 1
    ]
    active = [category for category, count in category_counts.items() if count]
    required_inputs = {
        "elo/python/nordic-combined/polars/excel365/races.csv",
        *(path for category in active for path in CATEGORY_CONTRACTS[category]["inputs"]),
    }
    workbooks = [str(CATEGORY_CONTRACTS[category]["workbook"]) for category in active]
    json_outputs = [
        output
        for category in active
        for output in _json_outputs(category, category_counts[category])
    ]
    if not events:
        status = "no_races"
    elif blockers:
        status = "blocked"
    else:
        status = "ready_for_shadow_implementation"
    return {
        "schema_version": SCHEMA_VERSION,
        "status": status,
        "publication_enabled": False,
        "logical_date": logical_date.isoformat(),
        "date_key": date_key,
        "events": [asdict(event) for event in events],
        "categories": category_counts,
        "required_ski_inputs": sorted(required_inputs),
        "model": {
            "entrypoint": "content/post/nordic-combined/drafts/race-picks-simulation.R",
            "workbooks": workbooks,
            "environment": {
                "NORDIC_INPUT_ROOT": "<isolated-ski>/elo/python/nordic-combined/polars/excel365",
                "NORDIC_TEAM_INPUT_ROOT": "<isolated-ski>/elo/python/nordic-combined/polars/relay/excel365",
                "NORDIC_OUTPUT_ROOT": "<isolated-output>/workbooks",
                "NORDIC_BLOG_ROOT": "<pinned-blog>",
                "NORDIC_LOG_ROOT": "<isolated-output>/logs",
                "NORDIC_LOGICAL_DATE": logical_date.isoformat(),
                "NORDIC_RUN_ID": "<run-id>",
                "NORDIC_RANDOM_SEED": "<integer-seed>",
                "NORDIC_N_SIMULATIONS": "<positive-integer>",
                "NORDIC_LOAD_ENV": "false",
            },
        },
        "conversion": {
            "entrypoint": "static/python/excel_to_hugo_multiple_sheets.py",
            "json_outputs": json_outputs,
        },
        "candidate": {
            "data_root": f"data/nordic-combined/drafts/race-picks/{date_key}",
            "post": f"content/post/race-picks/{date_key}.md",
            "optional_legacy_points_files": [],
        },
        "blockers": blockers,
    }


def _write_json(path: Path, document: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(
        json.dumps(document, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    temporary.replace(path)


def parse_args(argv: Iterable[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schedule", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--output", required=True, type=Path)
    return parser.parse_args(argv)


def main(argv: Iterable[str] | None = None) -> int:
    args = parse_args(argv)
    _write_json(args.output, build_plan(args.schedule, args.logical_date))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
