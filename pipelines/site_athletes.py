"""Build deterministic athlete indexes and metadata from accepted Elo history."""

from __future__ import annotations

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
