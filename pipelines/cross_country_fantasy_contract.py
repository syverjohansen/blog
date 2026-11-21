"""Side-effect-free contract for cross-country weekend fantasy teams.

The module deliberately performs no network requests and no optimization. It
validates captured Fantasy XC prices, prediction inputs, and optimized team
outputs so the runtime implementation cannot fall back to fuzzy name matching
or silently publish a partial fantasy product.
"""

from __future__ import annotations

from collections.abc import Iterable, Mapping
from dataclasses import asdict, dataclass
from decimal import Decimal, InvalidOperation

SCHEMA_VERSION = "nordicnumbers.cross-country-fantasy-plan/v1"
PRICE_SOURCE = "https://www.fantasyxc.se/api/athletes"
TEAM_SIZE = 16
ATHLETES_PER_SEX = 8
BUDGET = 100_000
OBJECTIVES = ("normal", "safe", "upside")
SCORE_FIELDS = {
    "normal": "total_points",
    "safe": "total_safe",
    "upside": "total_upside",
}
PREDICTION_FIELDS = frozenset(
    {
        "canonical_athlete_id",
        "fis_code",
        "canonical_name",
        "canonical_nation",
        "sex",
        *SCORE_FIELDS.values(),
    }
)
PRICE_FIELDS = frozenset(
    {"athlete_id", "name", "price", "country", "gender", "active", "is_team"}
)


class FantasyContractError(ValueError):
    """Raised when fantasy inputs or outputs violate the accepted contract."""


@dataclass(frozen=True)
class FantasyPlan:
    schema_version: str
    status: str
    price_source: str
    identity_key: str
    prediction_count: int
    priced_prediction_count: int
    eligible_men: int
    eligible_ladies: int
    objectives: tuple[str, ...]
    team_size: int
    athletes_per_sex: int
    budget: int
    missing_fis_codes: tuple[str, ...]
    weekend_publication_blocked: bool
    reason: str

    def to_dict(self) -> dict[str, object]:
        result = asdict(self)
        result["objectives"] = list(self.objectives)
        result["missing_fis_codes"] = list(self.missing_fis_codes)
        return result


def _required_fields(
    row: Mapping[str, object], required: frozenset[str], *, label: str
) -> None:
    if missing := sorted(required - row.keys()):
        raise FantasyContractError(f"{label} is missing fields: {', '.join(missing)}")


def _sex(value: object, *, label: str) -> str:
    normalized = str(value).strip().upper()
    aliases = {"M": "M", "L": "L", "F": "L"}
    try:
        return aliases[normalized]
    except KeyError as exc:
        raise FantasyContractError(f"{label} has unsupported sex: {value!r}") from exc


def _score(value: object, *, label: str) -> Decimal:
    try:
        result = Decimal(str(value))
    except InvalidOperation as exc:
        raise FantasyContractError(f"{label} is not numeric: {value!r}") from exc
    if not result.is_finite() or result < 0:
        raise FantasyContractError(f"{label} must be a finite non-negative number")
    return result


def _price(value: object, *, label: str) -> int:
    if isinstance(value, bool):
        raise FantasyContractError(f"{label} must be a positive integer")
    try:
        result = int(value)
    except (TypeError, ValueError) as exc:
        raise FantasyContractError(f"{label} must be a positive integer") from exc
    if str(result) != str(value).strip() or result <= 0:
        raise FantasyContractError(f"{label} must be a positive integer")
    return result


def build_fantasy_plan(
    *,
    prediction_rows: Iterable[Mapping[str, object]],
    price_rows: Iterable[Mapping[str, object]] | None,
    price_source: str = PRICE_SOURCE,
) -> FantasyPlan:
    """Validate captured inputs and decide whether fantasy optimization may run.

    An unavailable or invalid price capture blocks only the optional fantasy
    product. It never blocks the independently validated weekend-picks post.
    """

    if price_source != PRICE_SOURCE:
        raise FantasyContractError(f"unsupported price source: {price_source!r}")

    predictions: dict[str, tuple[str, str]] = {}
    for index, row in enumerate(prediction_rows, start=1):
        _required_fields(row, PREDICTION_FIELDS, label=f"prediction row {index}")
        canonical_id = str(row["canonical_athlete_id"]).strip()
        fis_code = str(row["fis_code"]).strip()
        sex = _sex(row["sex"], label=f"prediction row {index}")
        if not canonical_id or not fis_code.isdigit():
            raise FantasyContractError(
                f"prediction row {index} requires canonical ID and numeric FIS code"
            )
        if fis_code in predictions:
            raise FantasyContractError(f"duplicate prediction FIS code: {fis_code}")
        for objective, field in SCORE_FIELDS.items():
            _score(row[field], label=f"prediction row {index} {objective} score")
        predictions[fis_code] = (canonical_id, sex)

    common = {
        "schema_version": SCHEMA_VERSION,
        "price_source": price_source,
        "identity_key": "prediction.fis_code == fantasy_xc.athlete_id",
        "prediction_count": len(predictions),
        "objectives": OBJECTIVES,
        "team_size": TEAM_SIZE,
        "athletes_per_sex": ATHLETES_PER_SEX,
        "budget": BUDGET,
        "weekend_publication_blocked": False,
    }
    if price_rows is None:
        return FantasyPlan(
            **common,
            status="unavailable",
            priced_prediction_count=0,
            eligible_men=0,
            eligible_ladies=0,
            missing_fis_codes=tuple(sorted(predictions)),
            reason="Fantasy XC price capture is unavailable",
        )

    prices: dict[str, tuple[str, int]] = {}
    for index, row in enumerate(price_rows, start=1):
        _required_fields(row, PRICE_FIELDS, label=f"price row {index}")
        fis_code = str(row["athlete_id"]).strip()
        if not fis_code.isdigit():
            raise FantasyContractError(f"price row {index} has invalid athlete_id")
        if fis_code in prices:
            raise FantasyContractError(f"duplicate price FIS code: {fis_code}")
        if not isinstance(row["active"], bool) or not isinstance(row["is_team"], bool):
            raise FantasyContractError(
                f"price row {index} active/is_team values must be booleans"
            )
        if row["active"] and not row["is_team"]:
            prices[fis_code] = (
                _sex(row["gender"], label=f"price row {index}"),
                _price(row["price"], label=f"price row {index} price"),
            )

    matched: dict[str, tuple[str, int]] = {}
    for fis_code, (_, prediction_sex) in predictions.items():
        price = prices.get(fis_code)
        if price is None:
            continue
        price_sex, amount = price
        if prediction_sex != price_sex:
            raise FantasyContractError(
                f"sex mismatch for FIS code {fis_code}: "
                f"prediction={prediction_sex}, price={price_sex}"
            )
        matched[fis_code] = (prediction_sex, amount)

    missing = tuple(sorted(set(predictions) - set(matched)))
    men = sum(sex == "M" for sex, _ in matched.values())
    ladies = sum(sex == "L" for sex, _ in matched.values())
    if missing:
        status = "blocked"
        reason = "one or more predicted athletes lack an active exact FIS-code price"
    elif men < ATHLETES_PER_SEX or ladies < ATHLETES_PER_SEX:
        status = "blocked"
        reason = "priced prediction pool cannot satisfy the 8 men / 8 ladies roster"
    else:
        status = "candidate"
        reason = "captured prices and all three objective inputs are complete"
    return FantasyPlan(
        **common,
        status=status,
        priced_prediction_count=len(matched),
        eligible_men=men,
        eligible_ladies=ladies,
        missing_fis_codes=missing,
        reason=reason,
    )


def validate_team(
    *,
    objective: str,
    team_rows: Iterable[Mapping[str, object]],
) -> dict[str, object]:
    """Validate an optimizer result independently of the chosen solver."""

    if objective not in OBJECTIVES:
        raise FantasyContractError(f"unsupported objective: {objective!r}")
    rows = list(team_rows)
    if len(rows) != TEAM_SIZE:
        raise FantasyContractError(f"team must contain exactly {TEAM_SIZE} athletes")
    identities: set[str] = set()
    sexes: list[str] = []
    total_price = 0
    for index, row in enumerate(rows, start=1):
        _required_fields(
            row,
            frozenset({"fis_code", "sex", "price", "objective_points"}),
            label=f"team row {index}",
        )
        fis_code = str(row["fis_code"]).strip()
        if not fis_code.isdigit() or fis_code in identities:
            raise FantasyContractError(
                f"invalid or duplicate team FIS code: {fis_code}"
            )
        identities.add(fis_code)
        sexes.append(_sex(row["sex"], label=f"team row {index}"))
        total_price += _price(row["price"], label=f"team row {index} price")
        _score(row["objective_points"], label=f"team row {index} points")
    if sexes.count("M") != ATHLETES_PER_SEX or sexes.count("L") != ATHLETES_PER_SEX:
        raise FantasyContractError("team must contain exactly 8 men and 8 ladies")
    if total_price > BUDGET:
        raise FantasyContractError(f"team price {total_price} exceeds budget {BUDGET}")
    return {
        "schema_version": SCHEMA_VERSION,
        "objective": objective,
        "athlete_count": len(rows),
        "men": sexes.count("M"),
        "ladies": sexes.count("L"),
        "total_price": total_price,
        "budget": BUDGET,
        "status": "valid",
    }
