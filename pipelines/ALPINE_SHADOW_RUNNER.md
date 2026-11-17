# Alpine production-shaped shadow runner

`pipelines.alpine_shadow_runner` executes the current Alpine acquisition,
prediction, validation, and Hugo-render checks as one publication-disabled run.
It is intended for side-by-side validation beside the existing DigitalOcean
automation. It does not replace or invoke the publication path.

Example:

```bash
python3 -m pipelines.alpine_shadow_runner \
  --blog-root "$HOME/blog/daehl-e" \
  --ski-root "$HOME/ski" \
  --output-root "$HOME/nordic-shadow" \
  --logical-date 2026-11-28 \
  --expected-season 2027 \
  --n-simulations 50 \
  --allow-network
```

The output root must be outside both repositories. Each invocation creates a
unique directory under `OUTPUT_ROOT/runs/<run-id>/` containing:

- isolated acquisition inputs and their replay evidence;
- the content-addressed Parquet snapshot and materialized compatibility files;
- prediction workbooks and Hugo JSON;
- acquisition, simulation, and orchestration logs; and
- `manifests/alpine-run-manifest-final.json`.

The final manifest always records `publication.status` as `disabled`. A failed
acquisition or prediction run retains its logs and a failure manifest. Existing
run directories are never overwritten.

## Ubuntu scheduling

`pipelines/run_alpine_shadow_ubuntu.sh` is the production-shaped scheduling
entrypoint. It derives the logical date and FIS season in UTC, prevents
overlapping runs with `flock`, and passes `--skip-no-race` so quiet days write a
small publication-disabled skip manifest instead of failing.

The retired `master_automation_ubuntu.sh` delegates to this entrypoint for
compatibility with an existing cron registration. Do not also enable the
systemd timer until that cron entry has been removed, or the same day may run
twice.

User-level systemd templates are provided under `ops/systemd/user/`. To prepare
them on the Ubuntu host without enabling the timer:

```bash
mkdir -p "$HOME/.config/systemd/user" "$HOME/.config/nordic-numbers"
cp ops/systemd/user/nordic-alpine-shadow.* "$HOME/.config/systemd/user/"
cp ops/systemd/alpine-shadow.env.example \
  "$HOME/.config/nordic-numbers/alpine-shadow.env"
systemctl --user daemon-reload
systemctl --user start nordic-alpine-shadow.service
```

Inspect the retained run before scheduling it. After removing the legacy cron
entry, enable the timer with:

```bash
systemctl --user enable --now nordic-alpine-shadow.timer
```

For a user timer that must run while the account is logged out, enable lingering
once with `sudo loginctl enable-linger "$USER"`.

The runner expects the existing Python environment to contain both repositories'
integration dependencies, plus Rscript, the model's R packages, and Hugo
0.134.1. It reads the ski repository's accepted schedule and rating history,
but writes only below the configured output root.
