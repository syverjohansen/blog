# Cross-country fantasy contract audit

**Status:** Executable boundary defined; optimizer not yet ported

**Contract:** `nordicnumbers.cross-country-fantasy-plan/v1`

**Scope:** Phase 4 item 16, normal individual World Cup weekends

## Decision

Fantasy is an optional product built from the validated weekend prediction
run. A missing or invalid external price capture suppresses the fantasy
section with an explicit reason; it does not prevent the normal weekend picks
from being published.

The preserved legacy product contains three teams:

| Team | Objective field |
| --- | --- |
| Normal | `total_points` |
| Safe | `total_safe` |
| Upside | `total_upside` |

Each team contains exactly 16 unique athletes, exactly eight men and eight
ladies, and costs no more than 100,000. These are output invariants and are
validated independently of the eventual optimization solver.

## Price acquisition

The observed legacy source is `https://www.fantasyxc.se/api/athletes`. Its
current payload includes `athlete_id`, `name`, `price`, `country`, `gender`,
`active`, and `is_team`. `athlete_id` is the FIS code, so the accepted identity
join is:

```text
prediction.fis_code == fantasy_xc.athlete_id
```

Runtime fuzzy name matching is forbidden. Names remain display fields only.
The acquisition implementation must capture the response once per run with
its URL, retrieval time, SHA-256 digest, and raw payload. Only active,
non-team athletes are eligible. Duplicate IDs, invalid prices, sex conflicts,
or an unmatched predicted athlete block the fantasy candidate and require
visible evidence; they never silently remove an athlete from optimization.

Network access is deliberately absent from
`pipelines.cross_country_fantasy_contract`. The future acquisition step owns
timeouts/retries and passes a captured payload into this validator.

## Prediction prerequisite discovered by the audit

The current modern weekend workbook publishes the top 30 per sex and exposes
only normal expected points. That is sufficient for the existing weekend
post, but not for the preserved fantasy product. Before the optimizer is
ported, the model must emit a separate full-pool fantasy input containing:

- canonical athlete ID and FIS code;
- canonical name, nation, and sex;
- `total_points`, `total_safe`, and `total_upside`; and
- provenance linking the row to the validated weekend run.

The top-30 presentation workbook must not become the optimizer input. Safe
and Upside must be calculated by an explicit model definition; they cannot be
invented by renaming the normal score.

## Failure and publication behavior

| Condition | Fantasy result | Normal weekend post |
| --- | --- | --- |
| Price endpoint unavailable | `unavailable` | may publish |
| Missing exact FIS-code price | `blocked` | may publish |
| Invalid/duplicate price identity | validation failure | may publish |
| Missing Normal/Safe/Upside score | validation failure | may publish |
| Fewer than eight eligible athletes per sex | `blocked` | may publish |
| Invalid optimized roster or budget | validation failure | may publish without fantasy |

No stale fantasy team is carried into a new weekend. A later rerun may add or
replace the fantasy section after all gates pass.

## Specialized boundary

This contract covers normal individual World Cup weekends only. Relay/team
sprint fantasy, Tour de Ski overall, and Final Climb remain separate products
with distinct scoring and roster semantics.

## Next implementation slice

Extend the weekend model with the full-pool three-objective artifact and its
deterministic tests. After that artifact exists, add captured price
acquisition and the solver behind this contract.
