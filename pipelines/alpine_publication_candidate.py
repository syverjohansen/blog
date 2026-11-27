"""Materialize guarded Alpine publication files from a passed shadow run."""

from __future__ import annotations

import argparse
import json
import os
import tempfile
from datetime import UTC, date, datetime
from pathlib import Path
from typing import Any

from pipelines.alpine_publication_markdown import publication_markdown
from pipelines.alpine_smoke_evidence import write_json_atomic
from pipelines.test_pipeline import PipelineCheckError, validate_prediction_semantics

RESULT_EVIDENCE_FIELDS = ("Time", "Run1", "Run2", "Points")


def load_json(path: Path) -> Any:
    if not path.is_file():
        raise PipelineCheckError(f"required publication evidence is missing: {path}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise PipelineCheckError(f"invalid JSON in {path}: {exc}") from exc


def write_bytes_atomic(path: Path, payload: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.", suffix=".tmp", dir=path.parent
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(payload)
        os.replace(temporary, path)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise


def place_candidate_file(path: Path, payload: bytes) -> str:
    if path.exists():
        if path.read_bytes() != payload:
            raise PipelineCheckError(
                f"publication target already exists with different content: {path}"
            )
        return "unchanged"
    write_bytes_atomic(path, payload)
    return "created"


def require_passed_shadow(manifest: dict[str, Any]) -> tuple[date, list[str]]:
    if manifest.get("schema_version") != "nordicnumbers.alpine-production-shadow/v1":
        raise PipelineCheckError("unexpected production shadow manifest schema")
    if manifest.get("status") != "passed":
        raise PipelineCheckError("only a passed shadow run can become a candidate")
    if manifest.get("publication", {}).get("status") != "disabled":
        raise PipelineCheckError("shadow manifest did not retain the publication lock")
    for repository in ("blog", "ski"):
        state = manifest.get("code", {}).get(repository, {})
        if not state.get("commit_sha") or state.get("working_tree_dirty") is not False:
            raise PipelineCheckError(f"{repository} source provenance is not clean")
    failed = [
        item
        for item in manifest.get("task_outcomes", [])
        if item.get("status") != "passed"
    ]
    if failed:
        raise PipelineCheckError(f"shadow task outcomes are not all passed: {failed}")

    quality = manifest.get("quality_results", {})
    for gate in ("schema", "probability_bounds", "probability_monotonicity"):
        if quality.get(gate) != "passed":
            raise PipelineCheckError(f"required quality gate did not pass: {gate}")
    if quality.get("hugo_render", {}).get("status") != "passed":
        raise PipelineCheckError("Hugo candidate render did not pass")
    readiness = quality.get("current_input_readiness", {})
    if readiness.get("status") != "passed":
        raise PipelineCheckError("current input readiness did not pass")

    logical_date = date.fromisoformat(manifest["logical_date"])
    genders = list(readiness.get("genders", []))
    if not genders or not set(genders).issubset({"men", "ladies"}):
        raise PipelineCheckError(f"invalid publication genders: {genders}")
    return logical_date, genders


def require_safe_actual_results(
    actual_results: Any, races: list[dict[str, Any]]
) -> None:
    """Reject completed or ambiguous result pages while allowing validated proxies."""
    if not isinstance(actual_results, list):
        raise PipelineCheckError("actual results evidence must be a JSON array")
    if not actual_results:
        return

    for index, row in enumerate(actual_results):
        if not isinstance(row, dict):
            raise PipelineCheckError(
                f"actual results row {index} must be a JSON object"
            )
        if any(str(row.get(field) or "").strip() for field in RESULT_EVIDENCE_FIELDS):
            raise PipelineCheckError(
                "publication candidates cannot use a page containing completed results"
            )

    if any(race.get("source") != "historical_result_proxy" for race in races):
        raise PipelineCheckError(
            "nonempty result evidence is not a validated historical result proxy"
        )


def prepare_candidate(
    *,
    manifest_path: Path,
    replay_report_path: Path,
    actual_results_path: Path,
    site_data_root: Path,
    repository_root: Path,
    report_path: Path,
    evaluation_date: date | None = None,
) -> dict[str, Any]:
    manifest = load_json(manifest_path)
    logical_date, genders = require_passed_shadow(manifest)
    evaluation_date = evaluation_date or datetime.now(UTC).date()
    if logical_date < evaluation_date:
        raise PipelineCheckError(
            "publication candidates cannot be created for a past logical date"
        )
    replay = load_json(replay_report_path)
    actual_results = load_json(actual_results_path)

    if replay.get("status") != "passed":
        raise PipelineCheckError("startlist acquisition report did not pass")
    if replay.get("logical_date") != logical_date.isoformat():
        raise PipelineCheckError("startlist acquisition date does not match the run")
    if replay.get("publication", {}).get("status") != "disabled":
        raise PipelineCheckError("startlist acquisition did not disable publication")
    races = replay.get("races", [])
    if not races or any(
        race.get("result_fields_in_prediction_input") is not False for race in races
    ):
        raise PipelineCheckError("startlist evidence does not prove result isolation")
    require_safe_actual_results(actual_results, races)

    date_key = logical_date.strftime("%Y%m%d")
    source_root = site_data_root / "alpine/drafts/race-picks" / date_key
    target_root = repository_root / "data/alpine/drafts/race-picks" / date_key
    manifest_outputs = {
        output.get("file"): output for output in manifest.get("outputs", [])
    }
    planned: list[tuple[Path, bytes, dict[str, Any]]] = []
    for gender in genders:
        filename = f"{gender}_position_probabilities.json"
        source = source_root / filename
        semantic = validate_prediction_semantics(source)
        declared = manifest_outputs.get(filename)
        if not declared:
            raise PipelineCheckError(f"manifest does not declare output {filename}")
        if (
            declared.get("rows") != semantic["rows"]
            or declared.get("sha256") != semantic["sha256"]
            or declared.get("semantic_contract") != "passed"
        ):
            raise PipelineCheckError(
                f"generated output does not match manifest: {filename}"
            )
        planned.append((target_root / filename, source.read_bytes(), semantic))

    content_path = (
        repository_root
        / "data/race-picks/fragments"
        / date_key
        / "alpine.md"
    )
    planned.append(
        (
            content_path,
            publication_markdown(logical_date, genders),
            {"rows": None, "sha256": None},
        )
    )

    for target, payload, _ in planned:
        if target.exists() and target.read_bytes() != payload:
            raise PipelineCheckError(
                f"publication target already exists with different content: {target}"
            )

    files = []
    for target, payload, semantic in planned:
        state = place_candidate_file(target, payload)
        files.append(
            {
                "file": str(target.relative_to(repository_root)),
                "status": state,
                "rows": semantic["rows"],
                "sha256": semantic["sha256"],
            }
        )

    report = {
        "schema_version": "nordicnumbers.alpine-publication-candidate/v1",
        "status": "prepared"
        if any(item["status"] == "created" for item in files)
        else "unchanged",
        "logical_date": logical_date.isoformat(),
        "source_run_id": manifest.get("run_id"),
        "source_commits": manifest.get("code"),
        "files": files,
        "publication": {
            "status": "ready",
            "reason": "validated files are ready for guarded workflow publication",
        },
    }
    write_json_atomic(report_path, report)
    return report


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--replay-report", required=True, type=Path)
    parser.add_argument("--actual-results", required=True, type=Path)
    parser.add_argument("--site-data-root", required=True, type=Path)
    parser.add_argument("--repository-root", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        report = prepare_candidate(
            manifest_path=args.manifest,
            replay_report_path=args.replay_report,
            actual_results_path=args.actual_results,
            site_data_root=args.site_data_root,
            repository_root=args.repository_root,
            report_path=args.report,
            evaluation_date=None,
        )
    except (PipelineCheckError, KeyError, ValueError) as exc:
        print(str(exc), file=os.sys.stderr)
        return 1
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
