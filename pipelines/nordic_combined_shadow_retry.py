#!/usr/bin/env python3
"""Resolve scheduled shadow dates and retain official-startlist waiting evidence."""

from __future__ import annotations

import argparse
import csv
import json
import os
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any, Sequence


ACQUISITION_SCHEMA = "nordicnumbers.nordic-combined-startlist-acquisition/v1"
SHADOW_SCHEMA = "nordicnumbers.nordic-combined-prediction-shadow/v1"
PUBLICATION = {
    "status": "disabled",
    "reason": "shadow runs retain evidence and cannot create or publish posts",
}


class RetryContractError(ValueError):
    """Raised when retry evidence does not satisfy the shadow contract."""


def _parse_schedule_date(value: str) -> date:
    try:
        return datetime.strptime(value.strip(), "%m/%d/%Y").date()
    except ValueError as exc:
        raise RetryContractError(f"invalid schedule date: {value!r}") from exc


def resolve_logical_date(
    schedule_path: Path,
    *,
    requested: str = "",
    event_name: str = "workflow_dispatch",
    today: date | None = None,
    lookahead_days: int = 2,
) -> date:
    """Resolve manual runs exactly and scheduled runs to the next nearby race date."""
    if requested.strip():
        try:
            return date.fromisoformat(requested.strip())
        except ValueError as exc:
            raise RetryContractError(f"invalid requested logical date: {requested!r}") from exc

    current = today or datetime.now(timezone.utc).date()
    if event_name != "schedule":
        return current
    if lookahead_days < 0:
        raise RetryContractError("lookahead_days must be non-negative")

    try:
        with schedule_path.open(newline="", encoding="utf-8-sig") as handle:
            rows = list(csv.DictReader(handle))
    except OSError as exc:
        raise RetryContractError(f"cannot read schedule {schedule_path}: {exc}") from exc
    if not rows or "Date" not in rows[0]:
        raise RetryContractError("schedule is empty or missing Date")

    end = current + timedelta(days=lookahead_days)
    candidates = sorted(
        {
            scheduled
            for row in rows
            if current <= (scheduled := _parse_schedule_date(row.get("Date", ""))) <= end
        }
    )
    return candidates[0] if candidates else current


def load_acquisition_report(path: Path) -> dict[str, Any]:
    try:
        report = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise RetryContractError(f"cannot read acquisition report {path}: {exc}") from exc
    if report.get("schema_version") != ACQUISITION_SCHEMA:
        raise RetryContractError("unsupported acquisition report schema")
    return report


def classify_acquisition(report: dict[str, Any], *, step_outcome: str) -> str:
    """Distinguish an expected unpublished list from a broken acquisition."""
    status = report.get("status")
    eligible = report.get("prediction_eligible")
    if status == "ready" and eligible is True and step_outcome == "success":
        return "ready"
    if (
        status == "waiting"
        and eligible is False
        and report.get("official_event_count") == 0
        and report.get("entrant_row_count") == 0
        and step_outcome == "failure"
    ):
        return "waiting"
    raise RetryContractError(
        f"unexpected acquisition result: status={status!r}, "
        f"prediction_eligible={eligible!r}, step_outcome={step_outcome!r}"
    )


def write_waiting_report(
    path: Path,
    *,
    run_id: str,
    logical_date: str,
    blog_commit: str,
    ski_commit: str,
    acquisition_report_path: Path,
    acquisition_report: dict[str, Any],
) -> dict[str, Any]:
    report = {
        "schema_version": SHADOW_SCHEMA,
        "pipeline_id": "nordic_combined_prediction_shadow",
        "run_id": run_id,
        "logical_date": logical_date,
        "status": "skipped",
        "reason": "official_startlist_waiting",
        "code": {"blog_commit": blog_commit, "ski_commit": ski_commit},
        "evidence": {
            "acquisition_report": str(acquisition_report_path),
            "status": acquisition_report["status"],
            "scheduled_event_count": acquisition_report.get("scheduled_event_count", 0),
            "official_event_count": acquisition_report.get("official_event_count", 0),
            "unresolved_event_count": acquisition_report.get("unresolved_event_count", 0),
            "entrant_row_count": acquisition_report.get("entrant_row_count", 0),
        },
        "publication": dict(PUBLICATION),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    os.replace(temporary, path)
    return report


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    resolve = subparsers.add_parser("resolve-date")
    resolve.add_argument("--schedule", required=True, type=Path)
    resolve.add_argument("--requested", default="")
    resolve.add_argument("--event-name", required=True)
    resolve.add_argument("--today", type=date.fromisoformat)
    resolve.add_argument("--lookahead-days", type=int, default=2)

    classify = subparsers.add_parser("classify")
    classify.add_argument("--report", required=True, type=Path)
    classify.add_argument("--step-outcome", required=True)
    classify.add_argument("--github-output", type=Path)

    waiting = subparsers.add_parser("write-waiting")
    waiting.add_argument("--report", required=True, type=Path)
    waiting.add_argument("--output", required=True, type=Path)
    waiting.add_argument("--run-id", required=True)
    waiting.add_argument("--logical-date", required=True)
    waiting.add_argument("--blog-commit", required=True)
    waiting.add_argument("--ski-commit", required=True)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        if args.command == "resolve-date":
            resolved = resolve_logical_date(
                args.schedule,
                requested=args.requested,
                event_name=args.event_name,
                today=args.today,
                lookahead_days=args.lookahead_days,
            )
            print(resolved.isoformat())
        elif args.command == "classify":
            state = classify_acquisition(
                load_acquisition_report(args.report), step_outcome=args.step_outcome
            )
            if args.github_output:
                with args.github_output.open("a", encoding="utf-8") as handle:
                    handle.write(f"status={state}\n")
            print(state)
        else:
            acquisition = load_acquisition_report(args.report)
            if classify_acquisition(acquisition, step_outcome="failure") != "waiting":
                raise RetryContractError("acquisition is not waiting")
            write_waiting_report(
                args.output,
                run_id=args.run_id,
                logical_date=args.logical_date,
                blog_commit=args.blog_commit,
                ski_commit=args.ski_commit,
                acquisition_report_path=args.report,
                acquisition_report=acquisition,
            )
    except RetryContractError as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
