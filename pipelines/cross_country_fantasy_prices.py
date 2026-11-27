"""Capture and exactly enrich cross-country fantasy optimizer inputs."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import tempfile
import time
import unicodedata
from collections.abc import Callable
from datetime import UTC, date, datetime
from pathlib import Path
from typing import Any
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

from pipelines.cross_country_fantasy_contract import (
    PRICE_SOURCE,
    FantasyContractError,
    build_fantasy_plan,
)
from pipelines.cross_country_weekend_shadow import FANTASY_EXPECTED_HEADERS

CAPTURE_SCHEMA = "nordicnumbers.cross-country-fantasy-price-capture/v1"
PRICED_INPUT_SCHEMA = "nordicnumbers.cross-country-fantasy-priced-input/v2"
REPORT_SCHEMA = "nordicnumbers.cross-country-fantasy-price-report/v1"
SHEET_SEX = {"Men Full Pool": "M", "Ladies Full Pool": "L"}


class FantasyPriceError(RuntimeError):
    """Fantasy price acquisition or enrichment could not be completed."""


class FantasyPriceUnavailable(FantasyPriceError):
    """The external price source could not be read after bounded retries."""


def _sha256(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def _json_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"
    ).encode()


def _atomic_write(path: Path, content: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as handle:
        handle.write(content)
        handle.flush()
        os.fsync(handle.fileno())
        temporary = Path(handle.name)
    os.replace(temporary, path)


def fetch_price_payload(
    *,
    opener: Callable[..., Any] = urlopen,
    sleep: Callable[[float], None] = time.sleep,
    attempts: int = 3,
    timeout_seconds: int = 15,
) -> bytes:
    """Read the fixed public price endpoint with bounded transient retries."""

    if attempts < 1:
        raise ValueError("attempts must be positive")
    request = Request(
        PRICE_SOURCE,
        headers={"Accept": "application/json", "User-Agent": "NordicNumbers/1.0"},
    )
    last_error: Exception | None = None
    for attempt in range(1, attempts + 1):
        try:
            with opener(request, timeout=timeout_seconds) as response:
                status = getattr(response, "status", 200)
                if status != 200:
                    raise FantasyPriceUnavailable(f"Fantasy XC returned HTTP {status}")
                payload = response.read()
                if not payload:
                    raise FantasyPriceUnavailable("Fantasy XC returned an empty body")
                return payload
        except HTTPError as exc:
            last_error = exc
            if exc.code != 429 and not 500 <= exc.code <= 599:
                break
        except (URLError, TimeoutError, FantasyPriceUnavailable) as exc:
            last_error = exc
        if attempt < attempts:
            sleep(float(2 ** (attempt - 1)))
    raise FantasyPriceUnavailable(
        f"Fantasy XC price capture failed after {attempts} attempts: {last_error}"
    )


def _prediction_rows(
    *,
    workbook_path: Path,
    logical_date: date,
    random_seed: int,
    n_simulations: int,
) -> tuple[list[dict[str, object]], str]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise FantasyPriceError("openpyxl is required for fantasy enrichment") from exc
    try:
        workbook_bytes = workbook_path.read_bytes()
        workbook = load_workbook(workbook_path, read_only=True, data_only=True)
    except (OSError, ValueError) as exc:
        raise FantasyPriceError(f"cannot read fantasy input workbook: {exc}") from exc
    if not workbook.sheetnames or not set(workbook.sheetnames) <= set(SHEET_SEX):
        raise FantasyPriceError(
            f"unexpected fantasy input sheets: {sorted(workbook.sheetnames)}"
        )
    predictions: list[dict[str, object]] = []
    for sheet_name in workbook.sheetnames:
        rows = workbook[sheet_name].iter_rows(values_only=True)
        headers = tuple(next(rows, ()))
        if headers != FANTASY_EXPECTED_HEADERS:
            raise FantasyPriceError(f"unexpected headers in {sheet_name}")
        expected_sex = SHEET_SEX[sheet_name]
        for row in rows:
            (
                canonical_id,
                fis_code,
                name,
                nation,
                sex,
                normal,
                safe,
                upside,
                sprint,
                distance,
                races,
                sprint_races,
                distance_races,
                row_date,
                row_seed,
                row_simulations,
            ) = row
            if (
                sex != expected_sex
                or row_date != logical_date.isoformat()
                or row_seed != random_seed
                or row_simulations != n_simulations
            ):
                raise FantasyPriceError(
                    f"fantasy input provenance mismatch in {sheet_name}"
                )
            predictions.append(
                {
                    "canonical_athlete_id": str(canonical_id or ""),
                    "fis_code": str(fis_code or "").removesuffix(".0"),
                    "canonical_name": str(name or ""),
                    "canonical_nation": str(nation or ""),
                    "sex": sex,
                    "total_points": normal,
                    "total_safe": safe,
                    "total_upside": upside,
                    "sprint_points": sprint,
                    "distance_points": distance,
                    "races_count": races,
                    "sprint_races_count": sprint_races,
                    "distance_races_count": distance_races,
                }
            )
    workbook.close()
    return predictions, _sha256(workbook_bytes)


def _parse_prices(payload: bytes) -> list[dict[str, object]]:
    try:
        value = json.loads(payload)
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise FantasyContractError(f"price capture is not valid JSON: {exc}") from exc
    if not isinstance(value, list) or any(not isinstance(row, dict) for row in value):
        raise FantasyContractError("price capture must be a JSON array of objects")
    return value


def _normalized_name(value: object) -> tuple[str, ...]:
    """Return the same strict, order-independent tokens used by FIS identity."""

    transliteration = str.maketrans(
        {
            "æ": "ae",
            "ø": "oe",
            "œ": "oe",
            "ł": "l",
            "ð": "d",
            "þ": "th",
            "ß": "ss",
        }
    )
    decomposed = unicodedata.normalize(
        "NFKD", str(value).lower().translate(transliteration)
    )
    ascii_like = "".join(
        character for character in decomposed if not unicodedata.combining(character)
    )
    return tuple(sorted(re.findall(r"[a-z0-9]+", ascii_like)))


def _active_priced_predictions(
    predictions: list[dict[str, object]], prices: list[dict[str, object]]
) -> tuple[list[dict[str, object]], dict[str, int]]:
    """Select fantasy-eligible predictions without guessing identities.

    A supplied FIS code is authoritative and can only join that exact price row.
    Rows without a FIS code may be bridged to the active Fantasy XC feed only
    when sex plus normalized full-name tokens identify exactly one athlete.  The
    resolved FIS code is retained in the priced artifact so all downstream work
    continues to use the stable identifier rather than the display name.
    """

    active_prices = {
        str(row.get("athlete_id", "")).strip(): row
        for row in prices
        if row.get("active") is True and row.get("is_team") is False
    }
    names: dict[tuple[str, tuple[str, ...]], list[str]] = {}
    for fis_code, row in active_prices.items():
        gender = str(row.get("gender", "")).strip().upper()
        sex = {"M": "M", "F": "L", "L": "L"}.get(gender, "")
        tokens = _normalized_name(row.get("name", ""))
        if fis_code.isdigit() and sex and tokens:
            names.setdefault((sex, tokens), []).append(fis_code)

    resolved: list[dict[str, object]] = []
    exact_fis = 0
    exact_name = 0
    unavailable = 0
    ambiguous = 0
    for prediction in predictions:
        supplied_fis = str(prediction.get("fis_code", "")).strip()
        if supplied_fis:
            if supplied_fis in active_prices:
                resolved.append(
                    {**prediction, "identity_match_method": "exact_fis_code"}
                )
                exact_fis += 1
            else:
                # Never replace a present but conflicting FIS identity by name.
                unavailable += 1
            continue

        sex = str(prediction.get("sex", "")).strip().upper()
        candidates = names.get(
            (sex, _normalized_name(prediction.get("canonical_name", ""))), []
        )
        if len(candidates) == 1:
            resolved.append(
                {
                    **prediction,
                    "fis_code": candidates[0],
                    "identity_match_method": "exact_active_name_tokens",
                }
            )
            exact_name += 1
        elif candidates:
            ambiguous += 1
        else:
            unavailable += 1

    return resolved, {
        "input_rows": len(predictions),
        "active_price_rows": len(active_prices),
        "exact_fis_matches": exact_fis,
        "exact_name_bridges": exact_name,
        "unavailable_rows": unavailable,
        "ambiguous_name_rows": ambiguous,
        "eligible_rows": len(resolved),
    }


def _priced_rows(
    predictions: list[dict[str, object]], prices: list[dict[str, object]]
) -> list[dict[str, object]]:
    price_index = {
        str(row["athlete_id"]): row
        for row in prices
        if row["active"] and not row["is_team"]
    }
    enriched: list[dict[str, object]] = []
    for prediction in predictions:
        row = price_index[prediction["fis_code"]]
        enriched.append(
            {
                **prediction,
                "price": row["price"],
                "fantasy_xc_name": row["name"],
                "fantasy_xc_country": row["country"],
            }
        )
    return enriched


def capture_and_enrich_prices(
    *,
    workbook_path: Path,
    logical_date: date,
    random_seed: int,
    n_simulations: int,
    output_dir: Path,
    report_path: Path,
    opener: Callable[..., Any] = urlopen,
    sleep: Callable[[float], None] = time.sleep,
    now: Callable[[], datetime] = lambda: datetime.now(UTC),
) -> dict[str, Any]:
    """Capture one price snapshot and build an exact priced candidate."""

    predictions, workbook_sha = _prediction_rows(
        workbook_path=workbook_path.expanduser().resolve(),
        logical_date=logical_date,
        random_seed=random_seed,
        n_simulations=n_simulations,
    )
    output_dir = output_dir.expanduser().resolve()
    report_path = report_path.expanduser().resolve()
    retrieved_at = now().astimezone(UTC).isoformat().replace("+00:00", "Z")
    base: dict[str, Any] = {
        "schema_version": REPORT_SCHEMA,
        "logical_date": logical_date.isoformat(),
        "status": "unavailable",
        "publication": {"status": "disabled"},
        "weekend_publication_blocked": False,
        "source": {"url": PRICE_SOURCE, "retrieved_at": retrieved_at},
        "input": {
            "file": workbook_path.name,
            "sha256": workbook_sha,
            "rows": len(predictions),
        },
        "outputs": {},
    }
    try:
        payload = fetch_price_payload(opener=opener, sleep=sleep)
    except FantasyPriceUnavailable as exc:
        base["reason"] = str(exc)
        _atomic_write(report_path, _json_bytes(base))
        return base

    capture_path = output_dir / "fantasy_xc_prices.json"
    _atomic_write(capture_path, payload)
    capture_sha = _sha256(payload)
    base["source"].update({"sha256": capture_sha, "bytes": len(payload)})
    base["outputs"][capture_path.name] = {
        "schema_version": CAPTURE_SCHEMA,
        "sha256": capture_sha,
        "bytes": len(payload),
    }
    try:
        prices = _parse_prices(payload)
        eligible_predictions, identity_resolution = _active_priced_predictions(
            predictions, prices
        )
        base["identity_resolution"] = identity_resolution
        plan = build_fantasy_plan(
            prediction_rows=eligible_predictions,
            price_rows=prices,
            price_source=PRICE_SOURCE,
        )
    except FantasyContractError as exc:
        base.update(status="blocked", reason=str(exc))
        _atomic_write(report_path, _json_bytes(base))
        return base

    base["plan"] = plan.to_dict()
    if plan.status != "candidate":
        base.update(status=plan.status, reason=plan.reason)
        _atomic_write(report_path, _json_bytes(base))
        return base

    priced_document = {
        "schema_version": PRICED_INPUT_SCHEMA,
        "logical_date": logical_date.isoformat(),
        "publication": {"status": "disabled"},
        "price_source": {
            "url": PRICE_SOURCE,
            "retrieved_at": retrieved_at,
            "sha256": capture_sha,
        },
        "prediction_source": {
            "file": workbook_path.name,
            "sha256": workbook_sha,
            "random_seed": random_seed,
            "n_simulations": n_simulations,
        },
        "rows": _priced_rows(eligible_predictions, prices),
    }
    priced_bytes = _json_bytes(priced_document)
    priced_path = output_dir / "fantasy_priced_inputs.json"
    _atomic_write(priced_path, priced_bytes)
    base.update(status="candidate", reason=plan.reason)
    base["outputs"][priced_path.name] = {
        "schema_version": PRICED_INPUT_SCHEMA,
        "sha256": _sha256(priced_bytes),
        "rows": len(priced_document["rows"]),
    }
    _atomic_write(report_path, _json_bytes(base))
    return base


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--random-seed", required=True, type=int)
    parser.add_argument("--n-simulations", required=True, type=int)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    args = parser.parse_args()
    report = capture_and_enrich_prices(
        workbook_path=args.workbook,
        logical_date=args.logical_date,
        random_seed=args.random_seed,
        n_simulations=args.n_simulations,
        output_dir=args.output_dir,
        report_path=args.report,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
