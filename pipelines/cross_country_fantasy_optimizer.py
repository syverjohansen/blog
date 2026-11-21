"""Deterministically optimize validated cross-country fantasy inputs."""

from __future__ import annotations

import argparse
import bisect
import hashlib
import json
import os
import tempfile
from dataclasses import dataclass
from datetime import date
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any

from pipelines.cross_country_fantasy_contract import (
    ATHLETES_PER_SEX,
    BUDGET,
    OBJECTIVES,
    SCORE_FIELDS,
    FantasyContractError,
    validate_team,
)
from pipelines.cross_country_fantasy_prices import (
    PRICED_INPUT_SCHEMA,
)
from pipelines.cross_country_fantasy_prices import (
    REPORT_SCHEMA as PRICE_REPORT_SCHEMA,
)

TEAM_SCHEMA = "nordicnumbers.cross-country-fantasy-teams/v1"
REPORT_SCHEMA = "nordicnumbers.cross-country-fantasy-optimizer-report/v1"


class FantasyOptimizerError(RuntimeError):
    """Validated fantasy inputs cannot produce a trustworthy team set."""


@dataclass(frozen=True)
class _State:
    value: Decimal
    selected: tuple[int, ...]


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


def _read_json(path: Path, label: str) -> tuple[dict[str, Any], bytes]:
    try:
        content = path.read_bytes()
        value = json.loads(content)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise FantasyOptimizerError(f"cannot read {label}: {exc}") from exc
    if not isinstance(value, dict):
        raise FantasyOptimizerError(f"{label} must be a JSON object")
    return value, content


def _decimal(value: object, *, label: str) -> Decimal:
    try:
        result = Decimal(str(value))
    except InvalidOperation as exc:
        raise FantasyOptimizerError(f"{label} must be numeric") from exc
    if not result.is_finite() or result < 0:
        raise FantasyOptimizerError(f"{label} must be finite and non-negative")
    return result


def _price(value: object, *, label: str) -> int:
    if isinstance(value, bool):
        raise FantasyOptimizerError(f"{label} must be a positive integer")
    try:
        result = int(value)
    except (TypeError, ValueError) as exc:
        raise FantasyOptimizerError(f"{label} must be a positive integer") from exc
    if result <= 0 or str(result) != str(value).strip():
        raise FantasyOptimizerError(f"{label} must be a positive integer")
    return result


def _normalized_rows(document: dict[str, Any]) -> list[dict[str, Any]]:
    rows = document.get("rows")
    if not isinstance(rows, list) or not rows:
        raise FantasyOptimizerError("priced input contains no rows")
    required = {
        "canonical_athlete_id",
        "fis_code",
        "canonical_name",
        "canonical_nation",
        "sex",
        "price",
        *SCORE_FIELDS.values(),
    }
    normalized: list[dict[str, Any]] = []
    seen: set[str] = set()
    for index, raw in enumerate(rows, start=1):
        if not isinstance(raw, dict):
            raise FantasyOptimizerError(f"priced row {index} must be an object")
        if missing := sorted(required - raw.keys()):
            raise FantasyOptimizerError(
                f"priced row {index} is missing fields: {', '.join(missing)}"
            )
        fis_code = str(raw["fis_code"]).strip()
        sex = str(raw["sex"]).strip()
        if not fis_code.isdigit() or fis_code in seen:
            raise FantasyOptimizerError(
                f"priced row {index} has invalid or duplicate FIS code"
            )
        if sex not in {"M", "L"}:
            raise FantasyOptimizerError(f"priced row {index} has invalid sex")
        if not str(raw["canonical_athlete_id"]).strip():
            raise FantasyOptimizerError(f"priced row {index} has no canonical ID")
        if (
            not str(raw["canonical_name"]).strip()
            or not str(raw["canonical_nation"]).strip()
        ):
            raise FantasyOptimizerError(f"priced row {index} has invalid display data")
        row = dict(raw)
        row["fis_code"] = fis_code
        row["sex"] = sex
        row["price"] = _price(raw["price"], label=f"priced row {index} price")
        for objective, field in SCORE_FIELDS.items():
            row[field] = _decimal(
                raw[field], label=f"priced row {index} {objective} score"
            )
        seen.add(fis_code)
        normalized.append(row)
    return sorted(normalized, key=lambda row: row["fis_code"])


def _prefer(left: _State, right: _State) -> _State:
    if left.value != right.value:
        return left if left.value > right.value else right
    return left if left.selected < right.selected else right


def _prune(states: dict[int, _State]) -> dict[int, _State]:
    """Remove states made strictly unnecessary by a cheaper state."""

    result: dict[int, _State] = {}
    best: _State | None = None
    for cost in sorted(states):
        state = states[cost]
        if best is None or state.value > best.value:
            result[cost] = state
            best = state
        elif state.value == best.value and state.selected < best.selected:
            # Keep the cheaper state as the dominant budget choice. The new
            # selection is relevant only at this exact cost and cannot improve
            # a future budget, so it remains dominated.
            continue
    return result


def _sex_frontier(
    rows: list[dict[str, Any]], *, objective_field: str
) -> dict[int, _State]:
    states: list[dict[int, _State]] = [{} for _ in range(ATHLETES_PER_SEX + 1)]
    states[0][0] = _State(Decimal(0), ())
    for athlete_index, row in enumerate(rows):
        price = row["price"]
        value = row[objective_field]
        for count in range(ATHLETES_PER_SEX, 0, -1):
            for cost, previous in tuple(states[count - 1].items()):
                new_cost = cost + price
                if new_cost > BUDGET:
                    continue
                candidate = _State(
                    previous.value + value,
                    previous.selected + (athlete_index,),
                )
                existing = states[count].get(new_cost)
                states[count][new_cost] = (
                    candidate if existing is None else _prefer(existing, candidate)
                )
            states[count] = _prune(states[count])
    return states[ATHLETES_PER_SEX]


def _optimize_objective(
    rows: list[dict[str, Any]], *, objective: str
) -> tuple[list[dict[str, Any]], Decimal, int]:
    objective_field = SCORE_FIELDS[objective]
    by_sex = {sex: [row for row in rows if row["sex"] == sex] for sex in ("M", "L")}
    if any(len(pool) < ATHLETES_PER_SEX for pool in by_sex.values()):
        raise FantasyOptimizerError(
            "priced pool cannot satisfy the 8 men / 8 ladies roster"
        )
    men = _sex_frontier(by_sex["M"], objective_field=objective_field)
    ladies = _sex_frontier(by_sex["L"], objective_field=objective_field)
    if not men or not ladies:
        raise FantasyOptimizerError("no same-sex roster fits within the total budget")

    ladies_costs = sorted(ladies)
    best: tuple[Decimal, int, tuple[str, ...], _State, _State] | None = None
    for men_cost, men_state in men.items():
        index = bisect.bisect_right(ladies_costs, BUDGET - men_cost) - 1
        if index < 0:
            continue
        ladies_cost = ladies_costs[index]
        ladies_state = ladies[ladies_cost]
        selected_codes = tuple(
            sorted(
                [by_sex["M"][i]["fis_code"] for i in men_state.selected]
                + [by_sex["L"][i]["fis_code"] for i in ladies_state.selected]
            )
        )
        candidate = (
            men_state.value + ladies_state.value,
            men_cost + ladies_cost,
            selected_codes,
            men_state,
            ladies_state,
        )
        if (
            best is None
            or candidate[0] > best[0]
            or candidate[0] == best[0]
            and (
                candidate[1] < best[1]
                or (candidate[1] == best[1] and candidate[2] < best[2])
            )
        ):
            best = candidate
    if best is None:
        raise FantasyOptimizerError("no complete roster fits within the total budget")

    value, cost, _, men_state, ladies_state = best
    selected = [by_sex["M"][i] for i in men_state.selected] + [
        by_sex["L"][i] for i in ladies_state.selected
    ]
    team = [
        {
            "canonical_athlete_id": row["canonical_athlete_id"],
            "fis_code": row["fis_code"],
            "skier": row["canonical_name"],
            "nation": row["canonical_nation"],
            "sex": row["sex"],
            "price": row["price"],
            "objective_points": float(row[objective_field]),
        }
        for row in selected
    ]
    return team, value, cost


def optimize_fantasy_teams(
    *,
    price_report_path: Path,
    priced_input_path: Path,
    logical_date: date,
    output_path: Path,
    report_path: Path,
) -> dict[str, Any]:
    """Produce three validated teams or retain a non-blocking skip/block report."""

    price_report, _ = _read_json(price_report_path, "price report")
    base: dict[str, Any] = {
        "schema_version": REPORT_SCHEMA,
        "logical_date": logical_date.isoformat(),
        "status": "skipped",
        "publication": {"status": "disabled"},
        "weekend_publication_blocked": False,
        "outputs": {},
    }
    if price_report.get("schema_version") != PRICE_REPORT_SCHEMA:
        raise FantasyOptimizerError("unsupported price report schema")
    if price_report.get("logical_date") != logical_date.isoformat():
        raise FantasyOptimizerError("price report logical date does not match")
    if price_report.get("publication") != {"status": "disabled"}:
        raise FantasyOptimizerError("price report publication must remain disabled")
    if price_report.get("status") != "candidate":
        base["reason"] = (
            f"price enrichment is {price_report.get('status', 'invalid')}: "
            f"{price_report.get('reason', 'no candidate')}"
        )
        _atomic_write(report_path.expanduser().resolve(), _json_bytes(base))
        return base

    try:
        priced_document, priced_bytes = _read_json(
            priced_input_path, "priced fantasy input"
        )
        expected_sha = (
            price_report.get("outputs", {})
            .get(priced_input_path.name, {})
            .get("sha256")
        )
        if expected_sha != _sha256(priced_bytes):
            raise FantasyOptimizerError("priced input checksum does not match report")
        if priced_document.get("schema_version") != PRICED_INPUT_SCHEMA:
            raise FantasyOptimizerError("unsupported priced input schema")
        if priced_document.get("logical_date") != logical_date.isoformat():
            raise FantasyOptimizerError("priced input logical date does not match")
        if priced_document.get("publication") != {"status": "disabled"}:
            raise FantasyOptimizerError("priced input publication must remain disabled")
        rows = _normalized_rows(priced_document)
        teams: dict[str, Any] = {}
        for objective in OBJECTIVES:
            team_rows, objective_points, total_price = _optimize_objective(
                rows, objective=objective
            )
            validation = validate_team(objective=objective, team_rows=team_rows)
            teams[objective] = {
                "objective": objective,
                "objective_points": float(objective_points),
                "total_price": total_price,
                "validation": validation,
                "rows": team_rows,
            }
    except (FantasyOptimizerError, FantasyContractError) as exc:
        base.update(status="blocked", reason=str(exc))
        _atomic_write(report_path.expanduser().resolve(), _json_bytes(base))
        return base

    document = {
        "schema_version": TEAM_SCHEMA,
        "logical_date": logical_date.isoformat(),
        "publication": {"status": "disabled"},
        "priced_input": {
            "file": priced_input_path.name,
            "sha256": _sha256(priced_bytes),
        },
        "teams": teams,
    }
    output_bytes = _json_bytes(document)
    output_path = output_path.expanduser().resolve()
    _atomic_write(output_path, output_bytes)
    base.update(status="candidate", reason="all three optimized teams are valid")
    base["outputs"][output_path.name] = {
        "schema_version": TEAM_SCHEMA,
        "sha256": _sha256(output_bytes),
        "teams": len(teams),
    }
    _atomic_write(report_path.expanduser().resolve(), _json_bytes(base))
    return base


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--price-report", required=True, type=Path)
    parser.add_argument("--priced-input", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    args = parser.parse_args()
    report = optimize_fantasy_teams(
        price_report_path=args.price_report,
        priced_input_path=args.priced_input,
        logical_date=args.logical_date,
        output_path=args.output,
        report_path=args.report,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
