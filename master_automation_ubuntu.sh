#!/usr/bin/env bash
set -euo pipefail

# Retired compatibility entrypoint. Existing cron installations may continue
# invoking this path while the publication-disabled Alpine shadow runner is
# validated. Remove the old cron entry before enabling the systemd timer.
BLOG_ROOT="${NORDIC_BLOG_ROOT:-$HOME/blog/daehl-e}"

printf '%s\n' \
  'master_automation_ubuntu.sh is retired; delegating to the Alpine shadow runner.' \
  >&2
exec "$BLOG_ROOT/pipelines/run_alpine_shadow_ubuntu.sh" "$@"
