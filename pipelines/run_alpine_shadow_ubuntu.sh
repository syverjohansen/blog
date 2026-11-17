#!/usr/bin/env bash
set -euo pipefail

export TZ=UTC

BLOG_ROOT="${NORDIC_BLOG_ROOT:-$HOME/blog/daehl-e}"
SKI_ROOT="${NORDIC_SKI_ROOT:-$HOME/ski}"
OUTPUT_ROOT="${NORDIC_OUTPUT_ROOT:-$HOME/nordic-shadow}"
LOGICAL_DATE="${NORDIC_LOGICAL_DATE:-$(date -u '+%Y-%m-%d')}"
N_SIMULATIONS="${NORDIC_N_SIMULATIONS:-250}"

if [[ -n "${NORDIC_EXPECTED_SEASON:-}" ]]; then
  EXPECTED_SEASON="$NORDIC_EXPECTED_SEASON"
else
  year=$(date -u '+%Y')
  month=$(date -u '+%m')
  if ((10#$month >= 7)); then
    EXPECTED_SEASON=$((10#$year + 1))
  else
    EXPECTED_SEASON=$((10#$year))
  fi
fi

PYTHON_BIN="${NORDIC_PYTHON:-$HOME/blog/venv/bin/python3}"
if [[ ! -x "$PYTHON_BIN" ]]; then
  PYTHON_BIN=$(command -v python3)
fi

mkdir -p "$OUTPUT_ROOT"
LOCK_FILE="${NORDIC_LOCK_FILE:-$OUTPUT_ROOT/alpine-shadow.lock}"
exec 9>"$LOCK_FILE"
if ! flock -n 9; then
  printf 'Alpine shadow run already active; exiting without overlap.\n' >&2
  exit 0
fi

cd "$BLOG_ROOT"
exec "$PYTHON_BIN" -m pipelines.alpine_shadow_runner \
  --blog-root "$BLOG_ROOT" \
  --ski-root "$SKI_ROOT" \
  --output-root "$OUTPUT_ROOT" \
  --logical-date "$LOGICAL_DATE" \
  --expected-season "$EXPECTED_SEASON" \
  --n-simulations "$N_SIMULATIONS" \
  --allow-network \
  --skip-no-race
