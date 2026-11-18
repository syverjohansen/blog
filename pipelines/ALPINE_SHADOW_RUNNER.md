# Alpine production-shaped shadow runner

`pipelines.alpine_shadow_runner` executes the current Alpine acquisition,
prediction, validation, and Hugo-render checks as one publication-disabled run.
It is the read-only validation stage that must pass before the separate guarded
publication job can write prediction content.

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

## GitHub-hosted orchestration

`.github/workflows/alpine-site-data-update.yml` is the daily scheduler. After
updating accepted results and site data, it dispatches
`.github/workflows/alpine-production-shadow.yml` with the current logical date.
The prediction workflow can also be started manually. It resolves the ski
source to an exact commit, uses `$RUNNER_TEMP` as the isolated output root, and
uploads the complete retained run as a 30-day Actions artifact.

Quiet days still invoke the runner, but `--skip-no-race` writes only a small
publication-disabled skip manifest. R packages, model dependencies, and Hugo
are installed only on race days. The shadow job has read-only repository
permissions and contains no Git mutation or publication step.

## Guarded automatic publication

A workflow dispatch may set `publish_predictions` after a startlist is
available. Daily orchestration enables it; manual runs leave it disabled by
default. The shadow job still runs first with read-only permissions. A separate
write-scoped job runs only after every shadow gate passes and only for a
scheduled race date.

`pipelines/alpine_publication_candidate.py` then revalidates the retained
manifest, clean commit provenance, output checksums, input readiness, prediction
semantics, and Hugo render. It also refuses past logical dates, completed-result
evidence, or conflicting existing content. The job copies only the validated
position-probability JSON and deterministic race-picks page, renders the
complete repository, and commits those bounded paths directly to `main`. Any
failed gate or render prevents publication.

The runner expects the existing Python environment to contain both repositories'
integration dependencies, plus Rscript, the model's R packages, and Hugo
0.134.1. It reads the ski repository's accepted schedule and rating history,
but writes only below the configured output root.
