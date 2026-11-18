"""Build deterministic athlete indexes, metadata, and profile history chunks."""

from __future__ import annotations

from collections import defaultdict
from typing import Any


COUNTRY_CODES = {
    "Albania": "alb",
    "Algeria": "alg",
    "Andorra": "and",
    "Argentina": "arg",
    "Armenia": "arm",
    "Australia": "aus",
    "Austria": "aut",
    "Azerbaijan": "aze",
    "Belarus": "blr",
    "Belgium": "bel",
    "Bermuda": "ber",
    "Bolivia": "bol",
    "Bosnia&Herzegovina": "bih",
    "Brazil": "bra",
    "Bulgaria": "bul",
    "Cameroon": "cmr",
    "Canada": "can",
    "Cayman Islands": "cay",
    "Chile": "chi",
    "China": "chn",
    "Colombia": "col",
    "Costa Rica": "crc",
    "Croatia": "cro",
    "Cyprus": "cyp",
    "Czechia": "cze",
    "Denmark": "den",
    "East Timor": "tls",
    "Ecuador": "ecu",
    "Egypt": "egy",
    "Eritrea": "eri",
    "Estonia": "est",
    "Eswatini": "swz",
    "Ethiopia": "eth",
    "FIS": "fis",
    "Fiji": "fij",
    "Finland": "fin",
    "France": "fra",
    "Georgia": "geo",
    "Germany": "ger",
    "Ghana": "gha",
    "Great Britain": "gbr",
    "Greece": "gre",
    "Greenland": "grl",
    "Grenada": "grn",
    "Guam": "gum",
    "Guatemala": "gua",
    "Haiti": "hai",
    "Honduras": "hon",
    "Hong Kong": "hkg",
    "Hungary": "hun",
    "Iceland": "isl",
    "India": "ind",
    "Iran": "iri",
    "Ireland": "irl",
    "Israel": "isr",
    "Italy": "ita",
    "Jamaica": "jam",
    "Japan": "jpn",
    "Kazakhstan": "kaz",
    "Kenya": "ken",
    "Kosovo": "kos",
    "Kuwait": "kuw",
    "Kyrgyzstan": "kgz",
    "Latvia": "lat",
    "Lebanon": "lbn",
    "Liechtenstein": "lie",
    "Lithuania": "ltu",
    "Luxembourg": "lux",
    "Madagascar": "mad",
    "Malaysia": "mas",
    "Malta": "mlt",
    "Mexico": "mex",
    "Moldova": "mda",
    "Monaco": "mon",
    "Mongolia": "mgl",
    "Montenegro": "mne",
    "Morocco": "mar",
    "Nepal": "nep",
    "Netherlands": "ned",
    "New Zealand": "nzl",
    "Nigeria": "ngr",
    "North Korea": "prk",
    "North Macedonia": "mkd",
    "Norway": "nor",
    "Pakistan": "pak",
    "Peru": "per",
    "Philippines": "phi",
    "Poland": "pol",
    "Portugal": "por",
    "Puerto Rico": "pur",
    "Romania": "rou",
    "Russia": "rus",
    "San Marino": "smr",
    "Saudi Arabia": "ksa",
    "Senegal": "sen",
    "Serbia": "srb",
    "Singapore": "sgp",
    "Slovakia": "svk",
    "Slovenia": "slo",
    "South Africa": "rsa",
    "South Korea": "kor",
    "Soviet": "urs",
    "Spain": "esp",
    "Sweden": "swe",
    "Switzerland": "sui",
    "Taiwan": "tpe",
    "Tajikistan": "tjk",
    "Thailand": "tha",
    "Togo": "tog",
    "Tonga": "tga",
    "Trinidad & Tobago": "tto",
    "Turkey": "tur",
    "US Virgin Islands": "isv",
    "USA": "usa",
    "Ukraine": "ukr",
    "Uruguay": "uru",
    "Uzbekistan": "uzb",
    "Venezuela": "ven",
    "Yugoslavia": "yug",
    "Zimbabwe": "zim",
}


class AthleteMetadataError(RuntimeError):
    """Raised when accepted history cannot produce safe athlete metadata."""


def _athlete_id(row: dict[str, str]) -> int:
    try:
        athlete_id = int(float(row["ID"]))
    except (KeyError, TypeError, ValueError) as error:
        raise AthleteMetadataError(f"invalid athlete ID: {row.get('ID')!r}") from error
    if athlete_id <= 0:
        raise AthleteMetadataError(f"invalid athlete ID: {athlete_id}")
    return athlete_id


def _latest_rows(rows: list[dict[str, str]]) -> dict[int, dict[str, str]]:
    latest: dict[int, tuple[tuple[str, int, int, int], dict[str, str]]] = {}
    for index, row in enumerate(rows):
        athlete_id = _athlete_id(row)
        try:
            order = (
                row["Date"],
                int(float(row["Season"])),
                int(float(row["Race"])),
                index,
            )
        except (KeyError, TypeError, ValueError) as error:
            raise AthleteMetadataError(
                f"athlete {athlete_id} has invalid chronology"
            ) from error
        if athlete_id not in latest or order > latest[athlete_id][0]:
            latest[athlete_id] = (order, row)
    if not latest:
        raise AthleteMetadataError("athlete history is empty")
    return {athlete_id: value[1] for athlete_id, value in latest.items()}


def build_athlete_index(rows: list[dict[str, str]]) -> dict[str, str]:
    """Return every athlete ID mapped to the latest accepted display name."""
    output: dict[str, str] = {}
    for athlete_id, row in sorted(_latest_rows(rows).items()):
        name = row.get("Skier", "").strip()
        if not name:
            raise AthleteMetadataError(f"athlete {athlete_id} has no display name")
        output[str(athlete_id)] = name
    return output


def build_skier_info(rows: list[dict[str, str]], sex: str) -> list[dict[str, Any]]:
    """Return stable metadata records for every athlete in one accepted history."""
    if sex not in {"M", "L"}:
        raise AthleteMetadataError(f"unsupported sex: {sex}")

    output: list[dict[str, Any]] = []
    for athlete_id, row in sorted(_latest_rows(rows).items()):
        name = row.get("Skier", "").strip()
        nation = row.get("Nation", "").strip()
        if not name:
            raise AthleteMetadataError(f"athlete {athlete_id} has no display name")
        try:
            nation_code = COUNTRY_CODES[nation]
        except KeyError as error:
            raise AthleteMetadataError(
                f"athlete {athlete_id} has unsupported nation: {nation!r}"
            ) from error
        output.append(
            {
                "id": athlete_id,
                "skier": name,
                "sex": sex,
                "nation": nation_code,
                "image": f"/img/flags/{nation_code}.svg",
            }
        )
    return output


def athlete_group_key(athlete_id: int | str) -> str:
    """Return the legacy-compatible storage bucket for an athlete ID."""
    try:
        numeric_id = int(float(athlete_id))
    except (TypeError, ValueError) as error:
        raise AthleteMetadataError(f"invalid athlete ID: {athlete_id!r}") from error
    if numeric_id <= 0:
        raise AthleteMetadataError(f"invalid athlete ID: {numeric_id}")
    if numeric_id < 500:
        return f"0_to_499_{numeric_id // 100 * 100}"
    if numeric_id < 1000:
        return f"500_to_999_{(numeric_id - 500) // 100 * 100 + 500}"
    return f"1000_plus_{numeric_id // 500 * 500}"


def build_skier_lookup(rows: list[dict[str, str]]) -> list[dict[str, Any]]:
    """Return the browser lookup that maps each athlete to a history bucket."""
    output: list[dict[str, Any]] = []
    for athlete_id, row in sorted(_latest_rows(rows).items()):
        name = row.get("Skier", "").strip()
        nation = row.get("Nation", "").strip()
        if not name:
            raise AthleteMetadataError(f"athlete {athlete_id} has no display name")
        try:
            nation_code = COUNTRY_CODES[nation]
        except KeyError as error:
            raise AthleteMetadataError(
                f"athlete {athlete_id} has unsupported nation: {nation!r}"
            ) from error
        output.append(
            {
                "id": athlete_id,
                "skier": name,
                "nation": nation,
                "nation_code": nation_code,
                "group": athlete_group_key(athlete_id),
            }
        )
    return output


def _typed_value(
    column: str,
    value: str,
    *,
    integer_columns: set[str],
    float_columns: set[str],
) -> Any:
    value = value.strip()
    if not value:
        return None
    if column in integer_columns:
        return int(float(value))
    if column in float_columns:
        return float(value)
    return value


def build_grouped_athlete_histories(
    rows: list[dict[str, str]],
    *,
    elo_columns: list[str],
    integer_columns: set[str],
    float_columns: set[str],
) -> dict[str, dict[str, dict[str, list[Any]]]]:
    """Build legacy-compatible, column-oriented full histories by ID bucket."""
    if not rows:
        raise AthleteMetadataError("athlete history is empty")
    required = {"ID", "Date", "Season", "Race", *elo_columns}
    missing = sorted(required - set(rows[0]))
    if missing:
        raise AthleteMetadataError(f"athlete history is missing columns: {missing}")

    maxima: dict[tuple[int, int, str], float] = {}
    for row in rows:
        try:
            race_key = (int(float(row["Season"])), int(float(row["Race"])))
            for column in elo_columns:
                raw_value = row[column].strip()
                if raw_value:
                    value = float(raw_value)
                    key = (*race_key, column)
                    maxima[key] = max(maxima.get(key, value), value)
        except (KeyError, TypeError, ValueError) as error:
            raise AthleteMetadataError("athlete history has invalid rating data") from error

    by_athlete: dict[int, list[dict[str, Any]]] = defaultdict(list)
    for row in rows:
        athlete_id = _athlete_id(row)
        record = {
            column: _typed_value(
                column,
                value,
                integer_columns=integer_columns,
                float_columns=float_columns,
            )
            for column, value in row.items()
        }
        race_key = (int(float(row["Season"])), int(float(row["Race"])))
        for column in elo_columns:
            raw_value = row[column].strip()
            value = float(raw_value) if raw_value else None
            maximum = maxima.get((*race_key, column))
            record[f"{column}_Pct"] = (
                value / maximum * 100 if value is not None and maximum else 0
            )
        by_athlete[athlete_id].append(record)

    groups: dict[str, dict[str, dict[str, list[Any]]]] = defaultdict(dict)
    for athlete_id in sorted(by_athlete):
        athlete_rows = sorted(
            by_athlete[athlete_id],
            key=lambda row: (row["Date"], row["Season"], row["Race"]),
        )
        groups[athlete_group_key(athlete_id)][str(athlete_id)] = {
            column: [row[column] for row in athlete_rows]
            for column in athlete_rows[0]
        }
    return dict(sorted(groups.items()))
