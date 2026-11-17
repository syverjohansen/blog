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

The runner expects the existing Python environment to contain both repositories'
integration dependencies, plus Rscript, the model's R packages, and Hugo
0.134.1. It reads the ski repository's accepted schedule and rating history,
but writes only below the configured output root.
