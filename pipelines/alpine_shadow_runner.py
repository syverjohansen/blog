"""Run the Alpine acquisition-to-Hugo pipeline in an isolated shadow directory."""

from __future__ import annotations

import argparse
import csv
import json
import re
import subprocess
import sys
from collections.abc import Callable
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from pipelines.alpine_smoke_evidence import write_json_atomic
from pipelines.test_pipeline import PipelineCheckError, git_state, parse_iso_date

RUN_SCHEMA_VERSION = "nordicnumbers.alpine-production-shadow/v1"
SAFE_RUN_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")


class ShadowRunnerError(RuntimeError):
    """Raised when the production-shaped shadow run cannot safely continue."""


def ensure_outside_repository(path: Path, repository: Path) -> None:
    if path == repository or repository in path.parents:
        raise ShadowRunnerError(
            f"shadow output {path} must be outside repository {repository}"
        )


def require_file(path: Path) -> Path:
    if not path.is_file():
        raise ShadowRunnerError(f"required runtime file is missing: {path}")
    return path


def default_run_id(logical_date: str) -> str:
    timestamp = datetime.now(UTC).strftime("%Y%m%dT%H%M%SZ")
    return f"alpine-shadow-{logical_date.replace('-', '')}-{timestamp}"


def schedule_has_race(races_path: Path, logical_date: str) -> bool:
    target = parse_iso_date(logical_date).strftime("%m/%d/%Y")
    with races_path.open(newline="", encoding="utf-8-sig") as handle:
        rows = csv.DictReader(handle)
        if not {"Date", "Sex"}.issubset(rows.fieldnames or ()):
            raise ShadowRunnerError(
                f"Alpine schedule is missing Date or Sex: {races_path}"
            )
        return any(
            (row.get("Date") or "").strip() == target
            and (row.get("Sex") or "").strip() in {"M", "L"}
            for row in rows
        )


def run_logged(
    command: list[str],
    *,
    cwd: Path,
    log_path: Path,
    command_runner: Callable[..., subprocess.CompletedProcess[str]],
) -> subprocess.CompletedProcess[str]:
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("w", encoding="utf-8") as handle:
        return command_runner(
            command,
            cwd=cwd,
            text=True,
            stdout=handle,
            stderr=subprocess.STDOUT,
            check=False,
        )


def failure_manifest(
    *,
    run_id: str,
    logical_date: str,
    code: dict[str, Any],
    task_outcomes: list[dict[str, str]],
    logs: list[str],
    error: str,
) -> dict[str, Any]:
    return {
        "schema_version": RUN_SCHEMA_VERSION,
        "pipeline_id": "alpine_production_shadow",
        "run_id": run_id,
        "logical_date": logical_date,
        "status": "failed",
        "code": code,
        "task_outcomes": task_outcomes,
        "evidence": {"logs": logs},
        "error": error,
        "publication": {
            "status": "disabled",
            "reason": "production-shaped shadow runs cannot publish or mutate Git",
        },
    }


def run_shadow(
    *,
    blog_root: Path,
    ski_root: Path,
    output_root: Path,
    logical_date: str,
    expected_season: int,
    n_simulations: int,
    run_id: str,
    allow_network: bool,
    skip_no_race: bool = False,
    command_runner: Callable[..., subprocess.CompletedProcess[str]] = subprocess.run,
) -> Path:
    parse_iso_date(logical_date)
    if expected_season < 2000:
        raise ShadowRunnerError("--expected-season must be a four-digit year")
    if n_simulations < 1:
        raise ShadowRunnerError("--n-simulations must be positive")
    if not SAFE_RUN_ID.fullmatch(run_id):
        raise ShadowRunnerError(f"unsafe run ID: {run_id}")

    blog_root = blog_root.expanduser().resolve()
    ski_root = ski_root.expanduser().resolve()
    output_root = output_root.expanduser().resolve()
    ensure_outside_repository(output_root, blog_root)
    ensure_outside_repository(output_root, ski_root)

    ski_runtime = ski_root / "elo/python/alpine/polars"
    scraper = require_file(ski_runtime / "startlist-scrape-races.py")
    races = require_file(ski_runtime / "excel365/races.csv")
    men_history = require_file(ski_runtime / "excel365/men_chrono.csv")
    ladies_history = require_file(ski_runtime / "excel365/ladies_chrono.csv")
    identity_aliases = require_file(
        ski_runtime / "excel365/fis_identity_aliases.csv"
    )
    pipeline = require_file(blog_root / "pipelines/test_pipeline.py")

    run_root = output_root / "runs" / run_id
    if run_root.exists():
        raise ShadowRunnerError(f"shadow run already exists: {run_root}")
    run_root.mkdir(parents=True)
    logs_root = run_root / "logs"
    acquisition_root = run_root / "acquisition"
    reports_root = run_root / "reports"
    manifests_root = run_root / "manifests"
    workspace_root = run_root / "workspace"
    manifest_path = manifests_root / "alpine-run-manifest-final.json"
    report_path = reports_root / "alpine-current-smoke-report.json"
    acquisition_log = logs_root / "acquire-current-inputs.log"
    pipeline_log = logs_root / "run-current-smoke.log"

    code = {"blog": git_state(blog_root), "ski": git_state(ski_root)}
    tasks = [
        "schedule_gate",
        "acquire_current_inputs",
        "run_current_season_smoke",
    ]
    task_outcomes: list[dict[str, str]] = []
    log_references: list[str] = []

    if skip_no_race and not schedule_has_race(races, logical_date):
        write_json_atomic(
            manifest_path,
            {
                "schema_version": RUN_SCHEMA_VERSION,
                "pipeline_id": "alpine_production_shadow",
                "run_id": run_id,
                "logical_date": logical_date,
                "status": "skipped",
                "code": code,
                "task_outcomes": [
                    {"task_id": tasks[0], "status": "skipped"},
                    {"task_id": tasks[1], "status": "skipped"},
                    {"task_id": tasks[2], "status": "skipped"},
                ],
                "skip_reason": "no Alpine race is scheduled for the logical date",
                "publication": {
                    "status": "disabled",
                    "reason": "production-shaped shadow runs cannot publish or mutate Git",
                },
            },
        )
        return manifest_path

    task_outcomes.append({"task_id": tasks[0], "status": "passed"})

    acquisition_command = [
        sys.executable,
        str(scraper),
        "shadow",
        "--logical-date",
        logical_date,
        "--races",
        str(races),
        "--men-history",
        str(men_history),
        "--ladies-history",
        str(ladies_history),
        "--identity-aliases",
        str(identity_aliases),
        "--output-dir",
        str(acquisition_root),
    ]
    if allow_network:
        acquisition_command.append("--allow-network")
    acquired = run_logged(
        acquisition_command,
        cwd=ski_runtime,
        log_path=acquisition_log,
        command_runner=command_runner,
    )
    log_references.append(str(acquisition_log.relative_to(run_root)))
    if acquired.returncode != 0:
        task_outcomes.extend(
            [
                {"task_id": tasks[1], "status": "failed"},
                {"task_id": tasks[2], "status": "skipped"},
            ]
        )
        write_json_atomic(
            manifest_path,
            failure_manifest(
                run_id=run_id,
                logical_date=logical_date,
                code=code,
                task_outcomes=task_outcomes,
                logs=log_references,
                error="Alpine input acquisition failed; see retained log",
            ),
        )
        raise ShadowRunnerError(f"Alpine input acquisition failed: {acquisition_log}")
    task_outcomes.append({"task_id": tasks[1], "status": "passed"})

    pipeline_command = [
        sys.executable,
        str(pipeline),
        "alpine-current-smoke",
        "--blog-root",
        str(blog_root),
        "--ski-root",
        str(ski_root),
        "--ski-input-root",
        str(acquisition_root),
        "--ski-commit-sha",
        code["ski"]["commit_sha"],
        "--logical-date",
        logical_date,
        "--expected-season",
        str(expected_season),
        "--n-simulations",
        str(n_simulations),
        "--run-id",
        run_id,
        "--workspace-root",
        str(workspace_root),
        "--report",
        str(report_path),
        "--manifest",
        str(manifest_path),
    ]
    executed = run_logged(
        pipeline_command,
        cwd=blog_root,
        log_path=pipeline_log,
        command_runner=command_runner,
    )
    log_references.append(str(pipeline_log.relative_to(run_root)))
    if executed.returncode != 0 or not manifest_path.is_file():
        task_outcomes.append({"task_id": tasks[2], "status": "failed"})
        write_json_atomic(
            manifest_path,
            failure_manifest(
                run_id=run_id,
                logical_date=logical_date,
                code=code,
                task_outcomes=task_outcomes,
                logs=log_references,
                error="Alpine prediction smoke failed; see retained log and report",
            ),
        )
        raise ShadowRunnerError(f"Alpine prediction smoke failed: {pipeline_log}")

    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if (
        manifest.get("status") != "passed"
        or manifest.get("publication", {}).get("status") != "disabled"
    ):
        task_outcomes.append({"task_id": tasks[2], "status": "failed"})
        write_json_atomic(
            manifest_path,
            failure_manifest(
                run_id=run_id,
                logical_date=logical_date,
                code=code,
                task_outcomes=task_outcomes,
                logs=log_references,
                error="Alpine prediction manifest failed the shadow safety contract",
            ),
        )
        raise ShadowRunnerError(f"shadow manifest failed safety checks: {manifest_path}")
    manifest["schema_version"] = RUN_SCHEMA_VERSION
    manifest["pipeline_id"] = "alpine_production_shadow"
    manifest["task_outcomes"] = [
        {"task_id": tasks[0], "status": "passed"},
        {"task_id": tasks[1], "status": "passed"},
        *manifest.get("task_outcomes", []),
    ]
    manifest["evidence"] = {"logs": log_references}
    write_json_atomic(manifest_path, manifest)
    return manifest_path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--blog-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--ski-root", type=Path, default=Path.home() / "ski")
    parser.add_argument("--output-root", type=Path, required=True)
    parser.add_argument("--logical-date", required=True)
    parser.add_argument("--expected-season", type=int, required=True)
    parser.add_argument("--n-simulations", type=int, default=50)
    parser.add_argument("--run-id")
    parser.add_argument("--allow-network", action="store_true")
    parser.add_argument(
        "--skip-no-race",
        action="store_true",
        help="write a successful skip manifest when no Alpine race is scheduled",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    run_id = args.run_id or default_run_id(args.logical_date)
    try:
        manifest = run_shadow(
            blog_root=args.blog_root,
            ski_root=args.ski_root,
            output_root=args.output_root,
            logical_date=args.logical_date,
            expected_season=args.expected_season,
            n_simulations=args.n_simulations,
            run_id=run_id,
            allow_network=args.allow_network,
            skip_no_race=args.skip_no_race,
        )
    except (PipelineCheckError, ShadowRunnerError, ValueError) as exc:
        print(str(exc), file=sys.stderr)
        return 1
    print(manifest)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
