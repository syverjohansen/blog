# Cross-country weekend-picks contract audit

**Status:** Acquisition boundary implemented; publication remains disabled

**Contract:** `nordicnumbers.cross-country-weekend-plan/v1`

**Scope:** Phase 4 item 15, normal individual World Cup weekends

## Decision

The modern weekend-picks product will be a validated aggregate of the
individual races grouped by `weekends.csv:Date`. It is separate from:

- the already-modern daily individual and team prediction posts;
- fantasy-team optimization (Phase 4 item 16);
- Tour de Ski overall predictions (Phase 4 item 17);
- Final Climb handling (Phase 4 item 18); and
- the Monday results recap, which describes completed races rather than
  predicting an upcoming weekend.

Relay and team-sprint predictions remain owned by the existing team prediction
workflow. A team-only weekend group must not create an empty individual weekly
post.

## Observed legacy flow

| Stage | Legacy implementation | Observed behavior |
| --- | --- | --- |
| Trigger | `predict_script_ubuntu.sh` | Runs when the UTC date equals `weekends.csv:Date`; this field groups all `Race_Date` values in the weekend. |
| Individual startlists | `ski/.../startlist-scrape-weekend.py` | Reads FIS startlists for each grouped race and writes `startlist_weekend_men.csv` and `startlist_weekend_ladies.csv`. |
| Missing startlists | `create_fallback_startlist` in the same scraper | Generates a current-season/quota-based fallback instead of dropping the weekend. |
| Team startlists | delegated relay/team-sprint scrapers | Mixed into the old weekend command; now superseded by the hosted team-prediction workflow. |
| Simulation | `weekly-picks-simulation.R` and older `weekly-picks2.R` | Produces weekend points and also fantasy outputs, coupling two products. |
| Conversion | `excel_to_hugo_multiple_sheets.py` | Converts every workbook found in date-named draft directories. |
| Publication | `predict_script_ubuntu.sh` and `weekly_picks_processor.sh` | Two competing processors write date-named posts with different paths and shapes. |
| Tour/Final Climb | branches inside `predict_script_ubuntu.sh` | Specialized simulations share the legacy dispatcher but are not normal weekend picks. |

The old scripts use wall-clock time and hard-coded home directories, suppress
some subprocess failures, and can mark a product generated even when no
workbook exists. Those behaviors are not part of the accepted contract.

## Proposed hosted boundary

### Inputs owned by `ski`

- accepted `weekends.csv`, including `Date`, `Race_Date`, FIS startlist URL,
  sex, venue, distance, technique, and period;
- accepted men’s and ladies’ chronological Elo histories;
- exact FIS startlist responses when published; and
- a validated generated fallback with explicit provenance when they are not.

`ski` produces an isolated, checksummed weekend input bundle. No accepted
history or schedule is mutated by candidate generation. The blog consumer
validates that bundle and materializes an isolated per-race participation
matrix before the model is allowed to run.

### Outputs owned by `blog`

- candidate JSON under
  `data/cross-country/weekly-picks/<YYYYMMDD>/`;
- one candidate post at
  `content/post/weekly-picks/<YYYYMMDD>.md`;
- validation and parity reports retained as workflow evidence; and
- narrow publication only after contract tests and a full Hugo render pass.

The date is the UTC logical date and therefore the stable idempotency key. An
identical rerun must produce the same artifact paths and content. A no-race
week is a successful `no_work` result with no post.

## Startlist behavior

Official startlists are preferred. If one or more FIS startlists are not yet
published, the planner selects `generated_fallback`; it does not silently
remove those races. A later runtime slice must validate fallback athlete IDs,
sex, nation quotas, source history revision, and provenance before prediction.

The planner itself never enables publication. Publication remains disabled
until acquisition, simulation, output schema, parity, and Hugo gates are wired
in a later PR.

## Specialized boundaries

The first `Period == 2` group is classified as
`deferred_specialized/tour_de_ski_overall` and produces no normal-weekend
candidate. Fantasy optimization and Final Climb outputs are likewise excluded
from this contract. This prevents the first implementation slice from
silently redefining those products.

## Deterministic evidence

`tests/fixtures/cross_country_weekend_contract/` covers:

1. a normal weekend with complete official startlists;
2. a Tour de Ski opening window;
3. a weekend whose official startlists are not yet published; and
4. a no-race week.

`pipelines.cross_country_weekend_contract` is a side-effect-free planner used
only to make these decisions executable and reviewable. It performs no network
requests, simulation, repository writes, or publication.

## Consumer boundary

`pipelines.cross_country_weekend_consumer` verifies every bundle checksum,
logical date, event identity, canonical athlete identity, and official versus
fallback label. It then writes `weekends.csv` plus gender-specific startlist
matrices to an isolated runtime directory. Confirmed official events contain
deterministic `0`/`1` participation values; fallback events remain unset for
the prediction model to estimate. The consumer neither runs R nor publishes.

## Next implementation slice

Make `weekly-picks-simulation.R` consume this isolated runtime through explicit
environment paths, disable its fantasy branch, and retain its candidate output
for shadow validation. Fantasy, Tour overall, and Final Climb remain separate
PRs.
