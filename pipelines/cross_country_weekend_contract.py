"""Deterministic planning contract for cross-country weekend picks.

This module records the accepted boundary discovered in the retired Droplet
automation. It does not scrape, simulate, publish, or make network requests.
"""

from __future__ import annotations

import argparse
import json
from collections.abc import Iterable, Mapping
from dataclasses import asdict, dataclass
from datetime import date, datetime
from pathlib import Path
from urllib.parse import parse_qs, urlparse

SCHEMA_VERSION = "nordicnumbers.cross-country-weekend-plan/v1"
TEAM_DISTANCES = frozenset({"Rel", "Ts"})
REQUIRED_FIELDS = frozenset(
    {
        "Date",
        "Race_Date",
        "Startlist",
        "Sex",
        "City",
        "Country",
        "Distance",
        "Technique",
        "Period",
    }
)


class WeekendContractError(ValueError):
    """Raised when a schedule cannot satisfy the planning contract."""


@dataclass(frozen=True)
class WeekendPlan:
    schema_version: str
    status: str
    product: str
    logical_date: str
    trigger: str
    event_count: int
    individual_event_count: int
    team_event_count: int
    startlist_mode: str
    missing_startlist_race_ids: tuple[str, ...]
    candidate_data_root: str | None
    candidate_post_path: str | None
    acquisition_owner: str
    publication_owner: str
    publication_enabled: bool
    reason: str

    def to_dict(self) -> dict[str, object]:
        result = asdict(self)
        result["missing_startlist_race_ids"] = list(
            self.missing_startlist_race_ids
        )
        return result


def _parse_date(value: str, field: str) -> date:
    for date_format in ("%m/%d/%Y", "%m/%d/%y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), date_format).date()
        except ValueError:
            continue
    raise WeekendContractError(f"invalid {field}: {value!r}")


def _race_id(startlist_url: str) -> str:
    query = parse_qs(urlparse(startlist_url).query)
    race_ids = query.get("raceid", [])
    if len(race_ids) != 1 or not race_ids[0].isdigit():
        raise WeekendContractError(
            f"startlist URL has no single numeric raceid: {startlist_url!r}"
        )
    return race_ids[0]


def _validate_rows(rows: Iterable[Mapping[str, object]]) -> list[dict[str, str]]:
    normalized: list[dict[str, str]] = []
    seen: set[tuple[str, str]] = set()
    for index, raw_row in enumerate(rows, start=1):
        missing = sorted(REQUIRED_FIELDS - raw_row.keys())
        if missing:
            raise WeekendContractError(
                f"schedule row {index} is missing fields: {', '.join(missing)}"
            )
        row = {key: str(value).strip() for key, value in raw_row.items()}
        group_date = _parse_date(row["Date"], "Date")
        race_date = _parse_date(row["Race_Date"], "Race_Date")
        if race_date < group_date:
            raise WeekendContractError(
                f"race date precedes weekend group date in row {index}"
            )
        race_id = _race_id(row["Startlist"])
        identity = (row["Sex"], race_id)
        if identity in seen:
            raise WeekendContractError(
                f"duplicate weekend race identity: {row['Sex']}/{race_id}"
            )
        seen.add(identity)
        row["_group_date"] = group_date.isoformat()
        row["_race_id"] = race_id
        normalized.append(row)
    return normalized


def build_weekend_plan(
    *,
    schedule_rows: Iterable[Mapping[str, object]],
    logical_date: date,
    available_startlist_race_ids: Iterable[str],
) -> WeekendPlan:
    """Build a side-effect-free plan for one UTC logical date.

    ``Date`` is the weekend grouping/trigger date. ``Race_Date`` may span
    several days. Missing official startlists select the existing generated
    fallback path; they do not silently remove scheduled athletes or races.
    """

    rows = _validate_rows(schedule_rows)
    available = frozenset(str(value) for value in available_startlist_race_ids)
    selected = [row for row in rows if row["_group_date"] == logical_date.isoformat()]
    individual = [row for row in selected if row["Distance"] not in TEAM_DISTANCES]
    team = [row for row in selected if row["Distance"] in TEAM_DISTANCES]

    common = {
        "schema_version": SCHEMA_VERSION,
        "logical_date": logical_date.isoformat(),
        "trigger": "weekend_group_date",
        "event_count": len(selected),
        "individual_event_count": len(individual),
        "team_event_count": len(team),
        "acquisition_owner": "ski",
        "publication_owner": "blog",
        "publication_enabled": False,
    }
    if not selected:
        return WeekendPlan(
            **common,
            status="no_work",
            product="weekly_picks",
            startlist_mode="not_applicable",
            missing_startlist_race_ids=(),
            candidate_data_root=None,
            candidate_post_path=None,
            reason="no weekend group starts on the logical date",
        )

    period_two_dates = [
        date.fromisoformat(row["_group_date"])
        for row in rows
        if row["Period"] == "2"
    ]
    if period_two_dates and logical_date == min(period_two_dates):
        return WeekendPlan(
            **common,
            status="deferred_specialized",
            product="tour_de_ski_overall",
            startlist_mode="specialized_contract_required",
            missing_startlist_race_ids=(),
            candidate_data_root=None,
            candidate_post_path=None,
            reason="Tour de Ski opening belongs to Phase 4 item 17",
        )

    if not individual:
        return WeekendPlan(
            **common,
            status="no_supported_events",
            product="weekly_picks",
            startlist_mode="not_applicable",
            missing_startlist_race_ids=(),
            candidate_data_root=None,
            candidate_post_path=None,
            reason="team-only groups remain owned by the team prediction workflow",
        )

    required = sorted({row["_race_id"] for row in individual})
    missing = tuple(race_id for race_id in required if race_id not in available)
    compact_date = logical_date.strftime("%Y%m%d")
    return WeekendPlan(
        **common,
        status="candidate",
        product="weekly_picks",
        startlist_mode="generated_fallback" if missing else "official",
        missing_startlist_race_ids=missing,
        candidate_data_root=f"data/cross-country/weekly-picks/{compact_date}",
        candidate_post_path=f"content/post/weekly-picks/{compact_date}.md",
        reason=(
            "official startlists complete"
            if not missing
            else "one or more official startlists are unavailable; use validated fallback"
        ),
    )


def plan_scenario(path: Path) -> WeekendPlan:
    scenario = json.loads(path.read_text(encoding="utf-8"))
    return build_weekend_plan(
        schedule_rows=scenario["schedule"],
        logical_date=date.fromisoformat(scenario["logical_date"]),
        available_startlist_race_ids=scenario.get(
            "available_startlist_race_ids", []
        ),
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scenario", required=True, type=Path)
    args = parser.parse_args()
    print(json.dumps(plan_scenario(args.scenario).to_dict(), indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
