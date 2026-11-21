# Cross-country fantasy contract audit

**Status:** Price capture and exact enrichment implemented; optimizer not yet ported

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

Network access remains absent from
`pipelines.cross_country_fantasy_contract`. The hosted acquisition step uses
three bounded transient retries, retains the exact response and its SHA-256,
and passes that capture into the validator. No prior capture is reused when
the endpoint is unavailable.

On a complete exact join it writes `fantasy_priced_inputs.json` with the model
scores, captured price, both source checksums, and run provenance. An
unavailable endpoint or blocked join is retained as a report and exits without
stopping the normal weekend publication.

## Full-pool model artifact

The top-30 presentation workbook remains unchanged. The model also emits
`weekend_fantasy_inputs.xlsx`, with a full-pool sheet per available sex and:

- canonical athlete ID and FIS code;
- canonical name, nation, and sex;
- `total_points`, `total_safe`, and `total_upside`; and
- logical date, random seed, and simulation count linking every row to the
  validated weekend run.

Normal is the model mean. Safe and Upside are respectively the 10th and 90th
percentiles of the seeded Monte Carlo point distribution. Participation is
applied once to each race score before weekend aggregation. This replaces the
legacy implementation, which overwrote its first risk adjustment and then
applied participation a second time.

The validator checks the artifact and records incomplete FIS identities
without blocking the normal weekend post. Incomplete identity keeps the
fantasy candidate unavailable until an official exact identifier is present.

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

Add a deterministic solver for the Normal, Safe, and Upside teams, validate
all three rosters, and render the accepted fantasy section into the weekend
post.
