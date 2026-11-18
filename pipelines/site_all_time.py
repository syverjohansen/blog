"""Build deterministic all-time Elo records from accepted athlete history."""

from __future__ import annotations

from collections import defaultdict
import math
from typing import Any


class AllTimeEloError(RuntimeError):
    """Raised when accepted history cannot produce safe all-time records."""


def _athlete_id(row: dict[str, str]) -> int:
    try:
        athlete_id = int(float(row["ID"]))
    except (KeyError, TypeError, ValueError) as error:
        raise AllTimeEloError(f"invalid athlete ID: {row.get('ID')!r}") from error
    if athlete_id <= 0:
        raise AllTimeEloError(f"invalid athlete ID: {athlete_id}")
    return athlete_id


def _rating(value: str) -> float | None:
    value = value.strip()
    if value.lower() in {
        "", "none", "nan", "inf", "+inf", "-inf", "infinity", "+infinity",
        "-infinity",
    }:
        return None
    try:
        number = float(value)
    except ValueError as error:
        raise AllTimeEloError(f"invalid rating: {value!r}") from error
    return number if math.isfinite(number) else None


def build_all_time_elo(
    rows: list[dict[str, str]], rating_columns: dict[str, str]
) -> list[dict[str, Any]]:
    """Return one athlete record with each rating's maximum and first date."""
    if not rows:
        raise AllTimeEloError("athlete history is empty")
    required = {
        "ID", "Skier", "Nation", "Date", "Season", "Race", *rating_columns
    }
    missing = sorted(required - set(rows[0]))
    if missing:
        raise AllTimeEloError(f"athlete history is missing columns: {missing}")

    grouped: dict[int, list[tuple[int, dict[str, str]]]] = defaultdict(list)
    for index, row in enumerate(rows):
        grouped[_athlete_id(row)].append((index, row))

    records: list[dict[str, Any]] = []
    for athlete_id, athlete_rows in grouped.items():
        try:
            latest = max(
                athlete_rows,
                key=lambda item: (
                    item[1]["Date"],
                    int(float(item[1]["Season"])),
                    int(float(item[1]["Race"])),
                    item[0],
                ),
            )[1]
        except (TypeError, ValueError) as error:
            raise AllTimeEloError(
                f"athlete {athlete_id} has invalid chronology"
            ) from error
        skier = latest["Skier"].strip()
        nation = latest["Nation"].strip()
        if not skier:
            raise AllTimeEloError(f"athlete {athlete_id} has no display name")

        record: dict[str, Any] = {
            "Skier": skier,
            "Nation": nation,
        }
        dates: dict[str, str | None] = {}
        for source, published in rating_columns.items():
            candidates = []
            for index, row in athlete_rows:
                try:
                    chronology = (
                        row["Date"],
                        int(float(row["Season"])),
                        int(float(row["Race"])),
                        index,
                    )
                except (TypeError, ValueError) as error:
                    raise AllTimeEloError(
                        f"athlete {athlete_id} has invalid chronology"
                    ) from error
                candidates.append(
                    (_rating(row[source]), chronology, row["Date"])
                )
            finite = [candidate for candidate in candidates if candidate[0] is not None]
            if not finite:
                record[published] = None
                dates[f"{published}_Date"] = None
                continue
            maximum = max(candidate[0] for candidate in finite)
            first_maximum = min(
                (candidate for candidate in finite if candidate[0] == maximum),
                key=lambda candidate: candidate[1],
            )
            # Match the legacy pandas JSON contract and avoid meaningless
            # binary-float churn in the generated site artifacts.
            record[published] = round(maximum, 10)
            dates[f"{published}_Date"] = first_maximum[2] or None

        record["ID"] = athlete_id
        record.update(dates)
        records.append(record)

    overall_name = rating_columns.get("Elo")
    if overall_name is None:
        raise AllTimeEloError("rating columns must include Elo")
    records.sort(
        key=lambda record: (
            record[overall_name] is None,
            -(record[overall_name] or 0),
            record["ID"],
        )
    )
    for place, record in enumerate(records, start=1):
        record_with_place = {"Place": place}
        record_with_place.update(record)
        record.clear()
        record.update(record_with_place)
    return records
