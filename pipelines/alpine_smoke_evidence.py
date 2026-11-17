"""Write durable failure evidence for the Alpine hosted shadow workflow."""

from __future__ import annotations

import argparse
import json
import os
import tempfile
from pathlib import Path

SCHEMA_VERSION = "nordicnumbers.alpine-shadow-failure/v1"


def write_json_atomic(path: Path, payload: dict[str, object]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.",
        suffix=".tmp",
        dir=path.parent,
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            json.dump(payload, handle, indent=2, sort_keys=True)
            handle.write("\n")
        os.replace(temporary, path)
    except Exception:
        temporary.unlink(missing_ok=True)
        raise


def write_failure_evidence(
    *,
    output_path: Path,
    successful_manifest: Path,
    logical_date: str,
    blog_sha: str,
    ski_sha: str,
    task_statuses: dict[str, str],
    log_path: str,
) -> bool:
    """Write a failure manifest unless the successful run manifest exists."""

    if successful_manifest.is_file():
        return False

    write_json_atomic(
        output_path,
        {
            "schema_version": SCHEMA_VERSION,
            "status": "failed",
            "logical_date": logical_date,
            "code": {
                "blog": {"commit_sha": blog_sha},
                "ski": {"commit_sha": ski_sha or None},
            },
            "task_outcomes": [
                {"task_id": task_id, "status": status}
                for task_id, status in task_statuses.items()
            ],
            "evidence": {"scraper_log": log_path},
            "publication": {
                "status": "disabled",
                "reason": "shadow workflow failed before producing a successful manifest",
            },
        },
    )
    return True


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--successful-manifest", required=True, type=Path)
    parser.add_argument("--logical-date", required=True)
    parser.add_argument("--blog-sha", required=True)
    parser.add_argument("--ski-sha", default="")
    parser.add_argument("--scraper-log", required=True)
    parser.add_argument("--build-inputs-status", required=True)
    parser.add_argument("--handoff-status", required=True)
    parser.add_argument("--smoke-status", required=True)
    parser.add_argument("--smoke-gate-status", required=True)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    write_failure_evidence(
        output_path=args.output,
        successful_manifest=args.successful_manifest,
        logical_date=args.logical_date,
        blog_sha=args.blog_sha,
        ski_sha=args.ski_sha,
        task_statuses={
            "build_isolated_current_inputs": args.build_inputs_status,
            "enforce_scraper_handoff_contract": args.handoff_status,
            "run_current_season_smoke": args.smoke_status,
            "enforce_smoke_only_result": args.smoke_gate_status,
        },
        log_path=args.scraper_log,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
