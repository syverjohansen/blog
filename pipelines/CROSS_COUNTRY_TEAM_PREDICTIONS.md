# Cross-country team prediction runtime

The relay and team-sprint R scripts consume artifacts produced by the `ski`
repository and fail before writing workbooks when their prediction tables do
not satisfy `cross_country_team_prediction_contract.R`.

Hosted runs should set these paths explicitly:

- `NORDIC_BLOG_ROOT`: checkout root of this repository; the shared prediction
  contract is loaded from its `pipelines/` directory.
- `NORDIC_SKI_DATA_ROOT`: directory containing `races.csv` (and
  `test_races.csv` when `TEST_MODE=true`).
- `NORDIC_TEAM_DATA_ROOT`: directory containing chrono and validated team
  startlist CSVs.
- `NORDIC_PREDICTION_OUTPUT_ROOT`: parent directory for date-partitioned Excel
  prediction outputs.
- `NORDIC_LOG_ROOT`: prediction log directory.
- `NORDIC_ENV_FILE`: optional local environment file. Hosted runs should pass
  environment variables directly.

The scripts retain the historical home-directory locations as local defaults,
but automation must not rely on those defaults.

The output contract requires non-empty, matching points and probability tables,
unique team names, finite expected points, probabilities in `[0, 1]`, and the
row-wise relationship `Win <= Podium <= Top 5 <= Top 10`.

## Automatic publication

`.github/workflows/cross-country-team-predictions.yml` is chained after the
daily `Alpine Production Shadow` workflow instead of using an independent
clock. It checks the accepted cross-country schedule before installing R. A
day without a supported relay or team sprint exits successfully without doing
expensive work.

On a team-event day the workflow pins the ski repository to an exact commit,
materializes one complete men-and-women input per event type, runs the packaged
startlist builder and R model, validates the converted JSON, renders Hugo, and
pushes only that day's cross-country data directory and team-picks post. Logs,
event inputs, and workbooks are retained as workflow artifacts for 14 days.
