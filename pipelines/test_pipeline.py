#!/usr/bin/env python3
"""Deterministic orchestration and website smoke tests for Nordic Numbers.

This runner never invokes production scrapers, model jobs, Git, or deployment.
It verifies the trigger plan against a fixed schedule, validates the public
prediction contract, and builds a tiny isolated Hugo site using production
layouts and shortcodes.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
from contextlib import ExitStack
from dataclasses import dataclass
from datetime import date, datetime, timedelta, timezone
from pathlib import Path
from typing import Any

if __package__:
    from pipelines.alpine_publication_markdown import publication_markdown
else:
    # GitHub's deterministic gate also executes this file directly.
    from alpine_publication_markdown import publication_markdown


DATE_FORMAT = "%m/%d/%Y"
TASK_ORDER = (
    "schedule_refresh",
    "results_update",
    "race_predictions",
    "weekly_recap",
    "season_rebuild",
    "website_smoke",
)
PREDICTION_HEADERS = (
    "Skier",
    "ID",
    "Nation",
    "Sex",
    "Participation",
    "Win",
    "Podium",
    "Top-5",
    "Top-10",
    "Top-30",
)
ALPINE_RACE_HEADERS = (
    "Date",
    "Startlist",
    "Sex",
    "City",
    "Country",
    "Distance",
    "Period",
    "Championship",
    "Race",
)
MIN_ALPINE_STARTLIST_HISTORY_MATCH_RATE = 0.8


class PipelineCheckError(RuntimeError):
    """A deterministic pipeline check failed."""


@dataclass(frozen=True)
class Schedule:
    race_dates: frozenset[date]

    @property
    def first_race(self) -> date:
        return min(self.race_dates)

    @property
    def last_race(self) -> date:
        return max(self.race_dates)


def parse_iso_date(value: str) -> date:
    try:
        return date.fromisoformat(value)
    except ValueError as exc:
        raise PipelineCheckError(f"invalid ISO logical date: {value}") from exc


def load_schedule(path: Path) -> Schedule:
    if not path.is_file():
        raise PipelineCheckError(f"schedule does not exist: {path}")

    race_dates: set[date] = set()
    with path.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        if not reader.fieldnames or "Date" not in reader.fieldnames:
            raise PipelineCheckError(f"schedule is missing required Date column: {path}")
        for line_number, row in enumerate(reader, start=2):
            value = (row.get("Date") or "").strip()
            if not value:
                raise PipelineCheckError(f"blank Date at {path}:{line_number}")
            try:
                race_dates.add(datetime.strptime(value, DATE_FORMAT).date())
            except ValueError as exc:
                raise PipelineCheckError(
                    f"invalid Date {value!r} at {path}:{line_number}"
                ) from exc

    if not race_dates:
        raise PipelineCheckError(f"schedule contains no races: {path}")
    return Schedule(frozenset(race_dates))


def _task(status: str, dependencies: list[str] | None = None) -> dict[str, Any]:
    return {"status": status, "depends_on": dependencies or []}


def build_plan(logical_date: date, schedule: Schedule) -> dict[str, Any]:
    yesterday = logical_date - timedelta(days=1)
    has_race_today = logical_date in schedule.race_dates
    results_due = yesterday in schedule.race_dates
    in_season = schedule.first_race <= logical_date <= schedule.last_race + timedelta(days=1)
    is_monday = logical_date.weekday() == 0
    is_season_rebuild_day = (logical_date.month, logical_date.day) == (5, 1)

    run_results = in_season
    run_recap = in_season and is_monday
    run_site = has_race_today or results_due or run_recap or is_season_rebuild_day

    tasks: dict[str, dict[str, Any]] = {
        "schedule_refresh": _task("planned"),
        "results_update": _task(
            "planned" if run_results else "skipped", ["schedule_refresh"]
        ),
        "race_predictions": _task(
            "planned" if has_race_today else "skipped",
            ["results_update"],
        ),
        "weekly_recap": _task(
            "planned" if run_recap else "skipped",
            ["results_update"],
        ),
        # This deliberately does not depend on in-season status. The current
        # shell orchestrator exits before its May 1 check when the season has
        # ended, making the annual rebuild unreachable.
        "season_rebuild": _task(
            "planned" if is_season_rebuild_day else "skipped",
            ["schedule_refresh"],
        ),
        "website_smoke": _task("planned" if run_site else "skipped"),
    }

    producers: list[str] = []
    if results_due:
        producers.append("results_update")
    if has_race_today:
        producers.append("race_predictions")
    if run_recap:
        producers.append("weekly_recap")
    if is_season_rebuild_day:
        producers.append("season_rebuild")
    tasks["website_smoke"]["depends_on"] = producers

    return {
        "logical_date": logical_date.isoformat(),
        "schedule": {
            "first_race": schedule.first_race.isoformat(),
            "last_race": schedule.last_race.isoformat(),
            "race_count": len(schedule.race_dates),
        },
        "facts": {
            "in_season": in_season,
            "has_race_today": has_race_today,
            "results_due": results_due,
            "is_monday": is_monday,
            "is_season_rebuild_day": is_season_rebuild_day,
        },
        "tasks": tasks,
    }


def inject_failure(plan: dict[str, Any], failed_task: str) -> dict[str, Any]:
    if failed_task not in plan["tasks"]:
        raise PipelineCheckError(f"unknown task for failure injection: {failed_task}")
    if plan["tasks"][failed_task]["status"] != "planned":
        raise PipelineCheckError(f"cannot fail non-planned task: {failed_task}")

    copied = json.loads(json.dumps(plan))
    copied["tasks"][failed_task]["status"] = "failed"
    changed = True
    while changed:
        changed = False
        for task_name in TASK_ORDER:
            task = copied["tasks"][task_name]
            if task["status"] != "planned":
                continue
            dependency_statuses = [
                copied["tasks"][dependency]["status"]
                for dependency in task["depends_on"]
            ]
            if any(status in {"failed", "blocked"} for status in dependency_statuses):
                task["status"] = "blocked"
                changed = True
    return copied


def validate_prediction_file(path: Path) -> dict[str, Any]:
    if not path.is_file():
        raise PipelineCheckError(f"prediction fixture does not exist: {path}")
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise PipelineCheckError(f"invalid JSON in {path}: {exc}") from exc

    required_keys = {"sheet_name", "headers", "rows"}
    if set(document) != required_keys:
        raise PipelineCheckError(
            f"{path} keys must be {sorted(required_keys)}, got {sorted(document)}"
        )
    if tuple(document["headers"]) != PREDICTION_HEADERS:
        raise PipelineCheckError(
            f"{path} headers do not match prediction contract: {document['headers']}"
        )
    if not document["rows"]:
        raise PipelineCheckError(f"prediction fixture has no rows: {path}")
    width = len(document["headers"])
    for index, row in enumerate(document["rows"]):
        if len(row) != width:
            raise PipelineCheckError(
                f"row {index} in {path} has {len(row)} values; expected {width}"
            )
    return {"path": str(path), "rows": len(document["rows"]), "columns": width}


def _load_prediction_document(path: Path) -> dict[str, Any]:
    validate_prediction_file(path)
    return json.loads(path.read_text(encoding="utf-8"))


def validate_prediction_semantics(path: Path) -> dict[str, Any]:
    document = _load_prediction_document(path)
    headers = document["headers"]
    id_index = headers.index("ID")
    probability_indexes = [
        headers.index(column)
        for column in ("Win", "Podium", "Top-5", "Top-10", "Top-30")
    ]
    ids: set[str] = set()
    for row_number, row in enumerate(document["rows"], start=1):
        athlete_id = str(row[id_index]).removesuffix(".0")
        if not athlete_id or athlete_id in ids:
            raise PipelineCheckError(
                f"duplicate or blank athlete ID at row {row_number} in {path}"
            )
        ids.add(athlete_id)
        try:
            probabilities = [float(row[index]) for index in probability_indexes]
        except (TypeError, ValueError) as exc:
            raise PipelineCheckError(
                f"non-numeric probability at row {row_number} in {path}"
            ) from exc
        if any(value < 0 or value > 100 for value in probabilities):
            raise PipelineCheckError(
                f"probability outside [0, 100] at row {row_number} in {path}"
            )
        if probabilities != sorted(probabilities):
            raise PipelineCheckError(
                f"probabilities are not monotonic at row {row_number} in {path}"
            )
    return {
        "path": str(path),
        "rows": len(document["rows"]),
        "ids": sorted(ids),
        "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
    }


def compare_prediction_shape(generated: Path, baseline: Path) -> dict[str, Any]:
    generated_result = validate_prediction_semantics(generated)
    baseline_result = validate_prediction_semantics(baseline)
    if generated_result["rows"] != baseline_result["rows"]:
        raise PipelineCheckError(
            f"row count differs from baseline for {generated.name}: "
            f"{generated_result['rows']} != {baseline_result['rows']}"
        )
    if generated_result["ids"] != baseline_result["ids"]:
        generated_ids = set(generated_result["ids"])
        baseline_ids = set(baseline_result["ids"])
        raise PipelineCheckError(
            f"athlete IDs differ from baseline for {generated.name}; "
            f"missing={sorted(baseline_ids - generated_ids)}, "
            f"unexpected={sorted(generated_ids - baseline_ids)}"
        )
    return {
        "file": generated.name,
        "rows": generated_result["rows"],
        "sha256": generated_result["sha256"],
        "baseline_shape": "matched",
    }


def file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def git_state(root: Path) -> dict[str, Any]:
    revision = subprocess.run(
        ["git", "rev-parse", "HEAD"],
        cwd=root,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    status = subprocess.run(
        ["git", "status", "--porcelain"],
        cwd=root,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if revision.returncode != 0 or status.returncode != 0:
        raise PipelineCheckError(f"cannot capture Git state for {root}")
    return {
        "commit_sha": revision.stdout.strip(),
        "working_tree_dirty": bool(status.stdout.strip()),
    }


def resolve_ski_input(
    ski_root: Path,
    ski_input_root: Path | None,
    ski_commit_sha: str | None,
) -> tuple[Path, dict[str, Any]]:
    if ski_input_root is None:
        if ski_commit_sha:
            raise PipelineCheckError(
                "--ski-commit-sha requires --ski-input-root"
            )
        return ski_root / "elo/python/alpine/polars/excel365", git_state(ski_root)

    commit_sha = (ski_commit_sha or "").strip().lower()
    if not re.fullmatch(r"[0-9a-f]{40}", commit_sha):
        raise PipelineCheckError(
            "--ski-input-root requires a full 40-character --ski-commit-sha"
        )
    return ski_input_root, {
        "commit_sha": commit_sha,
        "working_tree_dirty": False,
        "source": "pinned-input-files",
    }


def _parse_input_date(value: str, *, source: str) -> date:
    for date_format in (DATE_FORMAT, "%Y-%m-%d"):
        try:
            return datetime.strptime(value, date_format).date()
        except ValueError:
            pass
    raise PipelineCheckError(f"invalid date {value!r} in {source}")


def validate_current_alpine_inputs(
    input_root: Path,
    logical_date: date,
    expected_season: int,
) -> dict[str, Any]:
    """Fail closed when current schedule and startlists are not aligned."""
    races_path = input_root / "races.csv"
    try:
        with races_path.open(newline="", encoding="utf-8") as handle:
            reader = csv.DictReader(handle)
            if tuple(reader.fieldnames or ()) != ALPINE_RACE_HEADERS:
                raise PipelineCheckError(
                    f"unexpected Alpine schedule columns: {reader.fieldnames}"
                )
            races = list(reader)
    except OSError as exc:
        raise PipelineCheckError(f"cannot read Alpine schedule: {exc}") from exc
    if not races:
        raise PipelineCheckError("Alpine schedule contains no races")

    dated_races = [
        (_parse_input_date(row["Date"], source="races.csv"), row)
        for row in races
    ]
    season_start = date(expected_season - 1, 7, 1)
    season_end = date(expected_season, 6, 30)
    outside = [
        race_date
        for race_date, _ in dated_races
        if not season_start <= race_date <= season_end
    ]
    if outside:
        raise PipelineCheckError(
            f"schedule dates fall outside FIS season {expected_season}: {outside[:3]}"
        )
    today = [row for race_date, row in dated_races if race_date == logical_date]
    if not today:
        raise PipelineCheckError(
            f"schedule has no Alpine race on logical date {logical_date}"
        )
    invalid_sexes = sorted({row["Sex"] for row in today} - {"M", "L"})
    if invalid_sexes:
        raise PipelineCheckError(f"invalid schedule sexes: {invalid_sexes}")

    gender_by_sex = {"M": "men", "L": "ladies"}
    genders = tuple(
        gender
        for sex, gender in gender_by_sex.items()
        if any(row["Sex"] == sex for row in today)
    )
    startlist_rows: dict[str, int] = {}
    startlist_sources: dict[str, list[str]] = {}
    startlist_ids: dict[str, set[str]] = {}
    for gender in genders:
        path = input_root / f"startlist_races_{gender}.csv"
        try:
            with path.open(newline="", encoding="utf-8") as handle:
                reader = csv.DictReader(handle)
                required = {"ID", "Skier", "Race_Date", "Startlist_Source"}
                if not required.issubset(reader.fieldnames or ()):
                    raise PipelineCheckError(
                        f"{path.name} is missing columns {sorted(required)}"
                    )
                rows = list(reader)
        except OSError as exc:
            raise PipelineCheckError(f"cannot read {path.name}: {exc}") from exc
        if not rows:
            raise PipelineCheckError(f"{path.name} contains no athletes")
        dates = {
            _parse_input_date(row["Race_Date"], source=path.name)
            for row in rows
            if (row.get("Race_Date") or "").strip()
        }
        if dates != {logical_date}:
            raise PipelineCheckError(
                f"{path.name} is stale or mixed for {logical_date}: "
                f"observed {sorted(value.isoformat() for value in dates)}"
            )
        if any(not (row.get("ID") or "").strip() for row in rows):
            raise PipelineCheckError(f"{path.name} contains blank athlete IDs")
        ids = {
            (row.get("ID") or "").strip().removesuffix(".0") for row in rows
        }
        if len(ids) != len(rows):
            raise PipelineCheckError(f"{path.name} contains duplicate athlete IDs")
        sources = sorted(
            {
                (row.get("Startlist_Source") or "").strip()
                for row in rows
            }
        )
        allowed_sources = {
            "historical_result_proxy",
            "generated_pre_race_history",
        }
        if not sources or not set(sources).issubset(allowed_sources):
            raise PipelineCheckError(
                f"{path.name} has invalid startlist provenance: {sources}"
            )
        startlist_rows[gender] = len(rows)
        startlist_sources[gender] = sources
        startlist_ids[gender] = ids

    chrono_rows: dict[str, int] = {}
    history_matches: dict[str, int] = {}
    history_match_rates: dict[str, float] = {}
    for gender in genders:
        path = input_root / f"{gender}_chrono.csv"
        try:
            with path.open(newline="", encoding="utf-8") as handle:
                reader = csv.DictReader(handle)
                required = {"ID", "Date", "Elo"}
                if not required.issubset(reader.fieldnames or ()):
                    raise PipelineCheckError(
                        f"{path.name} is missing columns {sorted(required)}"
                    )
                rows = list(reader)
        except OSError as exc:
            raise PipelineCheckError(f"cannot read {path.name}: {exc}") from exc
        if not rows:
            raise PipelineCheckError(f"{path.name} contains no rating history")
        history_ids = {
            (row.get("ID") or "").strip().removesuffix(".0")
            for row in rows
            if (row.get("ID") or "").strip()
        }
        matches = len(startlist_ids[gender] & history_ids)
        match_rate = matches / len(startlist_ids[gender])
        if match_rate < MIN_ALPINE_STARTLIST_HISTORY_MATCH_RATE:
            raise PipelineCheckError(
                f"{gender} startlist/history ID match rate {match_rate:.1%} "
                f"is below required {MIN_ALPINE_STARTLIST_HISTORY_MATCH_RATE:.1%}"
            )
        chrono_rows[gender] = len(rows)
        history_matches[gender] = matches
        history_match_rates[gender] = match_rate

    return {
        "status": "passed",
        "expected_season": expected_season,
        "logical_date": logical_date.isoformat(),
        "schedule_rows": len(races),
        "races_on_logical_date": len(today),
        "genders": list(genders),
        "startlist_rows": startlist_rows,
        "startlist_sources": startlist_sources,
        "chrono_rows": chrono_rows,
        "history_matches": history_matches,
        "history_match_rates": history_match_rates,
    }


def validate_fixture_contracts(blog_root: Path) -> list[dict[str, Any]]:
    data_root = blog_root / "tests/pipeline/fixtures/data/alpine/drafts/race-picks/20260324"
    return [
        validate_prediction_file(data_root / "men_position_probabilities.json"),
        validate_prediction_file(data_root / "ladies_position_probabilities.json"),
    ]


def load_scenarios(blog_root: Path) -> list[dict[str, Any]]:
    path = blog_root / "tests/pipeline/scenarios.json"
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise PipelineCheckError(f"cannot load scenarios from {path}: {exc}") from exc
    if not isinstance(document, list) or not document:
        raise PipelineCheckError(f"scenario file must contain a non-empty list: {path}")
    return document


def verify_scenarios(blog_root: Path) -> list[dict[str, Any]]:
    schedule_path = blog_root / "tests/pipeline/fixtures/races.csv"
    schedule = load_schedule(schedule_path)
    results: list[dict[str, Any]] = []
    for scenario in load_scenarios(blog_root):
        logical_date = parse_iso_date(scenario["logical_date"])
        plan = build_plan(logical_date, schedule)
        expected = scenario["expected"]
        errors: list[str] = []
        for fact, value in expected.get("facts", {}).items():
            actual = plan["facts"].get(fact)
            if actual != value:
                errors.append(f"fact {fact}: expected {value!r}, got {actual!r}")
        for task_name, status in expected.get("tasks", {}).items():
            actual = plan["tasks"].get(task_name, {}).get("status")
            if actual != status:
                errors.append(f"task {task_name}: expected {status!r}, got {actual!r}")
        if errors:
            raise PipelineCheckError(
                f"scenario {scenario['name']} failed: " + "; ".join(errors)
            )
        results.append(
            {
                "name": scenario["name"],
                "logical_date": scenario["logical_date"],
                "status": "passed",
            }
        )
    return results


def run_site_smoke(blog_root: Path) -> dict[str, Any]:
    hugo = shutil.which("hugo")
    if not hugo:
        raise PipelineCheckError("hugo is required for the website smoke test")

    with tempfile.TemporaryDirectory(prefix="nordic-numbers-site-smoke-") as temp_dir:
        destination = Path(temp_dir) / "site"
        command = [
            hugo,
            "--source",
            str(blog_root),
            "--config",
            "tests/pipeline/hugo.toml",
            "--destination",
            str(destination),
            "--cleanDestinationDir",
            "--noBuildLock",
            "--panicOnWarning",
            "--logLevel",
            "warn",
        ]
        completed = subprocess.run(
            command,
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if completed.returncode != 0:
            raise PipelineCheckError(
                "isolated Hugo build failed:\n" + completed.stdout.strip()
            )

        page = destination / "post/race-picks/alpine-shadow/index.html"
        if not page.is_file():
            raise PipelineCheckError(f"expected rendered page is missing: {page}")
        html = page.read_text(encoding="utf-8")
        expected_markers = (
            "Race Picks Test Fixture",
            "Marco Odermatt",
            "Mikaela Shiffrin",
            "table-alpinedraftsrace-picks20260324men_position_probabilities",
            "table-alpinedraftsrace-picks20260324ladies_position_probabilities",
        )
        missing = [marker for marker in expected_markers if marker not in html]
        if missing:
            rendered_table_ids = re.findall(r'id="(table-[^"]+)"', html)
            raise PipelineCheckError(
                "rendered page is missing expected markers: "
                f"{missing}; rendered table IDs: {rendered_table_ids}"
            )
        return {
            "status": "passed",
            "hugo": completed.args[0],
            "page": "post/race-picks/alpine-shadow/index.html",
            "bytes": page.stat().st_size,
        }


def run_generated_site_smoke(
    blog_root: Path,
    data_root: Path,
    logical_date: date,
    destination: Path | None = None,
    genders: tuple[str, ...] = ("men", "ladies"),
) -> dict[str, Any]:
    hugo = shutil.which("hugo")
    if not hugo:
        raise PipelineCheckError("hugo is required for the website smoke test")

    date_key = logical_date.strftime("%Y%m%d")
    with tempfile.TemporaryDirectory(prefix="nordic-numbers-generated-site-") as temp_dir:
        test_root = Path(temp_dir)
        content_root = test_root / "content"
        static_root = test_root / "static"
        site_destination = destination or (test_root / "site")
        page_source = content_root / "post/race-picks/alpine-integration.md"
        page_source.parent.mkdir(parents=True)
        static_root.mkdir()
        if not genders or not set(genders).issubset({"men", "ladies"}):
            raise PipelineCheckError(f"invalid genders for Hugo smoke: {genders}")
        page_source.write_bytes(publication_markdown(logical_date, list(genders)))
        config = test_root / "hugo.toml"
        config.write_text(
            'baseURL = "https://test.invalid/"\n'
            'title = "Nordic Numbers Integration Test"\n'
            'theme = "white"\n'
            'languageCode = "en-us"\n'
            f'contentDir = "{content_root.as_posix()}"\n'
            f'dataDir = "{data_root.as_posix()}"\n'
            f'staticDir = "{static_root.as_posix()}"\n'
            'disableKinds = ["home", "section", "taxonomy", "term", "RSS", "sitemap", "robotsTXT", "404"]\n\n'
            "[params]\n"
            '  header_image = ""\n'
            '  description = "Deterministic integration test"\n',
            encoding="utf-8",
        )
        completed = subprocess.run(
            [
                hugo,
                "--source",
                str(blog_root),
                "--config",
                str(config),
                "--destination",
                str(site_destination),
                "--cleanDestinationDir",
                "--noBuildLock",
                "--buildFuture",
                "--panicOnWarning",
                "--logLevel",
                "warn",
            ],
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if completed.returncode != 0:
            raise PipelineCheckError(
                "generated-data Hugo build failed:\n" + completed.stdout.strip()
            )
        page = site_destination / "post/race-picks/alpine-integration/index.html"
        if not page.is_file():
            raise PipelineCheckError(f"expected rendered page is missing: {page}")
        html = page.read_text(encoding="utf-8")
        expected_markers = (
            f"Race Picks for {logical_date.strftime('%m/%d/%Y')}",
        ) + tuple(
            f"table-alpinedraftsrace-picks{date_key}{gender}_position_probabilities"
            for gender in genders
        )
        missing = [marker for marker in expected_markers if marker not in html]
        if missing:
            raise PipelineCheckError(
                f"generated-data page is missing expected markers: {missing}"
            )
        return {
            "status": "passed",
            "page": "post/race-picks/alpine-integration/index.html",
            "bytes": page.stat().st_size,
        }


def run_alpine_integration(
    blog_root: Path,
    ski_root: Path,
    logical_date: date,
    n_simulations: int,
    *,
    ski_input_root: Path | None = None,
    ski_commit_sha: str | None = None,
    current_season: int | None = None,
    workspace_root: Path | None = None,
    run_id: str | None = None,
) -> dict[str, Any]:
    rscript = shutil.which("Rscript")
    if not rscript:
        raise PipelineCheckError("Rscript is required for the Alpine integration test")
    if n_simulations < 1:
        raise PipelineCheckError("--n-simulations must be positive")

    uv = shutil.which("uv")
    if uv:
        converter_prefix = [
            uv,
            "run",
            "--isolated",
            "--python",
            "3.12",
            "--with-requirements",
            str(blog_root / "pipelines/requirements-integration.txt"),
            "python",
        ]
        converter_runtime = "uv isolated Python 3.12"
    else:
        dependency_probe = subprocess.run(
            [sys.executable, "-c", "import pandas, numpy, openpyxl"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if dependency_probe.returncode != 0:
            raise PipelineCheckError(
                "the Excel converter requires pandas, numpy, and openpyxl; "
                "install the pinned integration requirements or install uv"
            )
        converter_prefix = [sys.executable]
        converter_runtime = sys.executable

    input_source, ski_code_state = resolve_ski_input(
        ski_root,
        ski_input_root,
        ski_commit_sha,
    )
    required_inputs = ("races.csv", "men_chrono.csv", "ladies_chrono.csv")
    missing_inputs = [name for name in required_inputs if not (input_source / name).is_file()]
    if missing_inputs:
        raise PipelineCheckError(
            f"Alpine integration inputs are missing from {input_source}: {missing_inputs}"
        )

    readiness = (
        validate_current_alpine_inputs(input_source, logical_date, current_season)
        if current_season is not None
        else None
    )
    if current_season is None:
        historical_startlists = (
            "startlist_races_men.csv",
            "startlist_races_ladies.csv",
        )
        missing_startlists = [
            name for name in historical_startlists if not (input_source / name).is_file()
        ]
        if missing_startlists:
            raise PipelineCheckError(
                "Alpine integration inputs are missing from "
                f"{input_source}: {missing_startlists}"
            )
    genders = (
        tuple(readiness["genders"])
        if readiness is not None
        else ("men", "ladies")
    )

    date_key = logical_date.strftime("%Y%m%d")
    run_kind = "current-smoke" if current_season is not None else "integration"
    run_id = run_id or f"alpine-{run_kind}-{date_key}"
    started = time.monotonic()
    started_at = datetime.now(timezone.utc).isoformat()
    code_state = {"blog": git_state(blog_root), "ski": ski_code_state}
    with ExitStack() as stack:
        if workspace_root is None:
            test_root = Path(
                stack.enter_context(
                    tempfile.TemporaryDirectory(
                        prefix="nordic-numbers-alpine-integration-"
                    )
                )
            )
            isolation = "temporary workspace; no production writes"
        else:
            test_root = workspace_root.expanduser().resolve()
            if test_root.exists():
                raise PipelineCheckError(
                    f"persistent Alpine workspace already exists: {test_root}"
                )
            test_root.mkdir(parents=True)
            isolation = f"persistent isolated workspace: {test_root}"
        input_root = test_root / "input"
        output_root = test_root / "products/race-picks"
        data_root = test_root / "site-data"
        snapshot_store = test_root / "object-store"
        snapshot_report_path = test_root / "snapshot-create.json"
        snapshot_script = blog_root / "pipelines/alpine_snapshot.py"
        snapshot_create_command = converter_prefix + [
                str(snapshot_script),
                "create",
                "--input-root",
                str(input_source),
                "--output-root",
                str(snapshot_store),
                "--logical-date",
                logical_date.isoformat(),
                "--report",
                str(snapshot_report_path),
            ]
        if current_season is not None:
            snapshot_create_command.append("--allow-partial-startlists")
        snapshot_create = subprocess.run(
            snapshot_create_command,
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if snapshot_create.returncode != 0 or not snapshot_report_path.is_file():
            raise PipelineCheckError(
                "Alpine Parquet snapshot creation failed:\n"
                + snapshot_create.stdout.strip()
            )
        snapshot_result = json.loads(snapshot_report_path.read_text(encoding="utf-8"))
        if snapshot_result.get("status") != "passed":
            raise PipelineCheckError(f"Alpine Parquet snapshot failed: {snapshot_result}")
        snapshot = snapshot_result["snapshot"]
        snapshot_dir = Path(snapshot["manifest_path"]).parent
        materialize_report_path = test_root / "snapshot-materialize.json"
        materialize = subprocess.run(
            converter_prefix
            + [
                str(snapshot_script),
                "materialize",
                "--snapshot-dir",
                str(snapshot_dir),
                "--output-root",
                str(input_root),
                "--report",
                str(materialize_report_path),
            ],
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if materialize.returncode != 0:
            raise PipelineCheckError(
                "Alpine Parquet snapshot materialization failed:\n"
                + materialize.stdout.strip()
            )

        env = os.environ.copy()
        env.update(
            {
                "NORDIC_LOAD_ENV": "false",
                "NORDIC_ENHANCED_LOGGING": "false",
                "NORDIC_INPUT_ROOT": str(input_root),
                "NORDIC_OUTPUT_ROOT": str(output_root),
                "NORDIC_LOG_ROOT": str(test_root / "logs"),
                "NORDIC_LOGICAL_DATE": logical_date.isoformat(),
                "NORDIC_RUN_ID": run_id,
                "NORDIC_RANDOM_SEED": date_key,
                "NORDIC_N_SIMULATIONS": str(n_simulations),
                "NORDIC_BLOG_ROOT": str(blog_root),
                "TEST_MODE": "false",
            }
        )
        simulation = subprocess.run(
            [rscript, str(blog_root / "content/post/alpine/drafts/race-picks-simulation.R")],
            cwd=blog_root,
            env=env,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
            timeout=600,
        )
        if simulation.returncode != 0:
            tail = "\n".join(simulation.stdout.splitlines()[-80:])
            raise PipelineCheckError(f"Alpine R simulation failed:\n{tail}")

        excel_root = output_root / date_key
        json_root = data_root / "alpine/drafts/race-picks" / date_key
        converter = blog_root / "static/python/excel_to_hugo_multiple_sheets.py"
        for gender in genders:
            workbook = excel_root / f"{gender}_position_probabilities.xlsx"
            if not workbook.is_file():
                raise PipelineCheckError(f"simulation did not create {workbook}")
            converted = subprocess.run(
                converter_prefix + [str(converter), str(workbook), str(json_root)],
                cwd=blog_root,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                check=False,
            )
            if converted.returncode != 0:
                raise PipelineCheckError(
                    f"Excel-to-JSON conversion failed for {workbook.name}:\n"
                    + converted.stdout.strip()
                )

        contracts = []
        baseline_root = blog_root / "data/alpine/drafts/race-picks" / date_key
        for gender in genders:
            filename = f"{gender}_position_probabilities.json"
            generated = json_root / filename
            if current_season is not None:
                semantic = validate_prediction_semantics(generated)
                contracts.append(
                    {
                        "file": filename,
                        "rows": semantic["rows"],
                        "sha256": semantic["sha256"],
                        "semantic_contract": "passed",
                    }
                )
            else:
                contracts.append(
                    compare_prediction_shape(generated, baseline_root / filename)
                )
        site_smoke = run_generated_site_smoke(
            blog_root,
            data_root,
            logical_date,
            genders=genders,
        )
        duration_seconds = round(time.monotonic() - started, 2)
        snapshot_manifest = {
            key: value
            for key, value in snapshot.items()
            if key not in {"manifest_path", "status"}
        }
        task_ids = (
            [
                "validate_current_input_readiness",
                "snapshot_csv_inputs_to_parquet",
                "verify_and_materialize_snapshot",
                "simulate_predictions",
                "convert_excel_to_json",
                "validate_prediction_contracts",
                "render_hugo_candidate",
            ]
            if current_season is not None
            else [
                "snapshot_csv_inputs_to_parquet",
                "verify_and_materialize_snapshot",
                "simulate_predictions",
                "convert_excel_to_json",
                "validate_prediction_contracts",
                "compare_with_accepted_output",
                "render_hugo_candidate",
            ]
        )
        task_outcomes = [
            {"task_id": task_id, "status": "passed"}
            for task_id in task_ids
        ]
        parameters = {
            "random_seed": date_key,
            "n_simulations": n_simulations,
            "converter_runtime": converter_runtime,
        }
        quality_results = {
            "schema": "passed",
            "probability_bounds": "passed",
            "probability_monotonicity": "passed",
            "accepted_athlete_set_and_row_count": "passed",
            "hugo_render": site_smoke,
        }
        if current_season is not None:
            parameters["expected_season"] = current_season
            quality_results["accepted_athlete_set_and_row_count"] = "not_applicable"
            quality_results["current_input_readiness"] = readiness

        run_manifest = {
            "schema_version": "nordicnumbers.pipeline-run/v1",
            "pipeline_id": (
                "alpine_current_season_smoke"
                if current_season is not None
                else "alpine_race_prediction_shadow"
            ),
            "run_id": run_id,
            "logical_date": logical_date.isoformat(),
            "started_at": started_at,
            "completed_at": datetime.now(timezone.utc).isoformat(),
            "status": "passed",
            "code": code_state,
            "parameters": parameters,
            "input_snapshot": snapshot_manifest,
            "task_outcomes": task_outcomes,
            "outputs": contracts,
            "quality_results": quality_results,
            "publication": {
                "status": "disabled",
                "reason": "shadow integration runs cannot publish or mutate Git",
            },
            "duration_seconds": duration_seconds,
        }
        result = {
            "status": "passed",
            "run_id": run_id,
            "logical_date": logical_date.isoformat(),
            "random_seed": date_key,
            "n_simulations": n_simulations,
            "converter_runtime": converter_runtime,
            "snapshot_id": snapshot["snapshot_id"],
            "contracts": contracts,
            "site_smoke": site_smoke,
            "duration_seconds": duration_seconds,
            "isolation": isolation,
            "run_manifest": run_manifest,
        }
        if current_season is not None:
            result["current_input_readiness"] = readiness
        return result


def write_report(report: dict[str, Any], path: Path | None) -> None:
    payload = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if path is None:
        sys.stdout.write(payload)
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(payload, encoding="utf-8")
    print(path)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "command",
        choices=(
            "plan",
            "verify",
            "site-smoke",
            "alpine-integration",
            "alpine-current-smoke",
            "all",
        ),
        help="pipeline check to run",
    )
    parser.add_argument(
        "--logical-date",
        help="ISO date for plan mode",
    )
    parser.add_argument(
        "--schedule",
        type=Path,
        help="CSV schedule for plan mode; defaults to the deterministic fixture",
    )
    parser.add_argument(
        "--fail-task",
        choices=TASK_ORDER,
        help="inject a failure into a planned task and show downstream blocking",
    )
    parser.add_argument(
        "--blog-root",
        type=Path,
        default=Path(__file__).resolve().parents[1],
    )
    parser.add_argument(
        "--ski-root",
        type=Path,
        default=Path.home() / "ski",
        help="ski repository root for the real-data Alpine integration test",
    )
    parser.add_argument(
        "--ski-input-root",
        type=Path,
        help="pre-fetched Alpine input directory for hosted parity runs",
    )
    parser.add_argument(
        "--ski-commit-sha",
        help="full ski commit SHA associated with --ski-input-root",
    )
    parser.add_argument(
        "--n-simulations",
        type=int,
        default=250,
        help="Monte Carlo iterations for the Alpine integration test",
    )
    parser.add_argument(
        "--expected-season",
        type=int,
        help="required FIS season for alpine-current-smoke",
    )
    parser.add_argument(
        "--report",
        type=Path,
        help="optional report path; stdout is used by default",
    )
    parser.add_argument(
        "--manifest",
        type=Path,
        help="optional standalone run-manifest path for alpine-integration",
    )
    parser.add_argument(
        "--workspace-root",
        type=Path,
        help="optional persistent isolated workspace for Alpine artifacts",
    )
    parser.add_argument(
        "--run-id",
        help="optional explicit Alpine run ID",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    blog_root = args.blog_root.resolve()
    try:
        if args.command == "plan":
            if not args.logical_date:
                raise PipelineCheckError("--logical-date is required for plan mode")
            schedule_path = args.schedule or (
                blog_root / "tests/pipeline/fixtures/races.csv"
            )
            plan = build_plan(parse_iso_date(args.logical_date), load_schedule(schedule_path))
            if args.fail_task:
                plan = inject_failure(plan, args.fail_task)
            report: dict[str, Any] = {"status": "passed", "plan": plan}
        elif args.command == "verify":
            report = {
                "status": "passed",
                "scenarios": verify_scenarios(blog_root),
                "contracts": validate_fixture_contracts(blog_root),
            }
        elif args.command == "site-smoke":
            report = {"status": "passed", "site_smoke": run_site_smoke(blog_root)}
        elif args.command in {"alpine-integration", "alpine-current-smoke"}:
            logical_date = parse_iso_date(args.logical_date or "2026-03-25")
            if args.command == "alpine-current-smoke" and args.expected_season is None:
                raise PipelineCheckError(
                    "--expected-season is required for alpine-current-smoke"
                )
            result_key = args.command.replace("-", "_")
            report = {
                "status": "passed",
                result_key: run_alpine_integration(
                    blog_root,
                    args.ski_root.expanduser().resolve(),
                    logical_date,
                    args.n_simulations,
                    ski_input_root=(
                        args.ski_input_root.expanduser().resolve()
                        if args.ski_input_root
                        else None
                    ),
                    ski_commit_sha=args.ski_commit_sha,
                    current_season=(
                        args.expected_season
                        if args.command == "alpine-current-smoke"
                        else None
                    ),
                    workspace_root=(
                        args.workspace_root.expanduser().resolve()
                        if args.workspace_root
                        else None
                    ),
                    run_id=args.run_id,
                ),
            }
            if args.manifest:
                write_report(
                    report[result_key]["run_manifest"],
                    args.manifest,
                )
        else:
            report = {
                "status": "passed",
                "scenarios": verify_scenarios(blog_root),
                "contracts": validate_fixture_contracts(blog_root),
                "site_smoke": run_site_smoke(blog_root),
            }
        write_report(report, args.report)
        return 0
    except PipelineCheckError as exc:
        write_report({"status": "failed", "error": str(exc)}, args.report)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
