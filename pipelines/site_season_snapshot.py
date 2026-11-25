"""Shared active-season snapshot rules for Nordic Numbers site archives."""

from __future__ import annotations

from collections.abc import Mapping


class SeasonSnapshotError(RuntimeError):
    """Raised when a season cannot produce a deterministic snapshot."""


def _order(row: dict[str, str]) -> tuple[str, int]:
    return row["Date"], int(row["Race"])


def current_season_rows(
    rows: list[dict[str, str]], season: int
) -> dict[str, dict[str, str]]:
    """Return each current-season racer's latest actual race row."""
    latest: dict[str, dict[str, str]] = {}
    for row in rows:
        if int(row["Season"]) != season or row["Event"] == "Offseason":
            continue
        athlete_id = row["ID"]
        if athlete_id not in latest or _order(row) > _order(latest[athlete_id]):
            latest[athlete_id] = row
    if not latest:
        raise SeasonSnapshotError(f"season {season} contains no active racers")
    return latest


def build_season_snapshot(
    rows: list[dict[str, str]],
    season: int,
    rating_pairs: Mapping[str, str],
    *,
    extra_metadata: Mapping[str, str] | None = None,
) -> list[dict[str, str]]:
    """Compare the prior undiscounted close with each latest current rating.

    ``rating_pairs`` maps the published previous-rating column (for example
    ``Pelo``) to its current-rating column (``Elo``). Athletes without a prior
    offseason row use the rating immediately before their first current-season
    race as their comparison baseline.
    """
    latest = current_season_rows(rows, season)
    first: dict[str, dict[str, str]] = {}
    for row in rows:
        if (
            int(row["Season"]) == season
            and row["Event"] != "Offseason"
            and (row["ID"] not in first or _order(row) < _order(first[row["ID"]]))
        ):
            first[row["ID"]] = row

    prior = [
        row
        for row in rows
        if row["Event"] == "Offseason" and int(row["Season"]) < season
    ]
    if not prior:
        raise SeasonSnapshotError(f"no offseason ratings exist before season {season}")
    rollover_season = max(int(row["Season"]) for row in prior)
    prior_by_id = {
        row["ID"]: row for row in prior if int(row["Season"]) == rollover_season
    }

    metadata = {
        "Date": f"{season}-05-01",
        "City": "Summer",
        "Country": "Break",
        "Event": "Offseason",
        "Distance": "0",
        "Place": "0",
        "Season": str(season),
        "Race": "0",
        **(extra_metadata or {}),
    }
    snapshot: list[dict[str, str]] = []
    for athlete_id, latest_row in latest.items():
        output = latest_row.copy()
        output.update(metadata)
        baseline = prior_by_id.get(athlete_id)
        for previous_column, current_column in rating_pairs.items():
            starting_value = (baseline or {}).get(previous_column, "").strip() or first[
                athlete_id
            ].get(previous_column, "").strip()
            current_value = latest_row.get(current_column, "").strip()
            if current_value and not starting_value:
                raise SeasonSnapshotError(
                    f"{athlete_id} has {current_column} without a starting "
                    f"{previous_column}"
                )
            output[previous_column] = starting_value
            output[current_column] = current_value
        snapshot.append(output)
    return snapshot
