"""Build a publication-disabled Hugo candidate from a validated weekend shadow."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from datetime import date, datetime
from pathlib import Path
from typing import Any

from pipelines.cross_country_fantasy_contract import OBJECTIVES, validate_team
from pipelines.cross_country_fantasy_optimizer import (
    REPORT_SCHEMA as FANTASY_REPORT_SCHEMA,
)
from pipelines.cross_country_fantasy_optimizer import TEAM_SCHEMA as FANTASY_TEAM_SCHEMA
from pipelines.cross_country_weekend_shadow import EXPECTED_HEADERS

CANDIDATE_SCHEMA = "nordicnumbers.cross-country-weekend-candidate/v1"
RUNTIME_SCHEMA = "nordicnumbers.cross-country-weekend-runtime/v1"
VALIDATION_SCHEMA = "nordicnumbers.cross-country-weekend-validation/v1"
RENDER_SCHEMA = "nordicnumbers.cross-country-weekend-render/v1"
SHEET_OUTPUTS = {"Men Top 30": ("men.json", "M"), "Ladies Top 30": ("ladies.json", "L")}
FANTASY_LABELS = {
    "normal": "Expected Points",
    "safe": "Safer Team",
    "upside": "Upside Team",
}
FANTASY_HEADERS = ("Skier", "Nation", "Sex", "Price", "Projected Points")


class WeekendPublicationError(RuntimeError):
    """A validated shadow cannot be safely rendered as a Hugo candidate."""


def _read_json(path: Path, label: str) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise WeekendPublicationError(f"cannot read {label}: {exc}") from exc
    if not isinstance(value, dict):
        raise WeekendPublicationError(f"{label} must be a JSON object")
    return value


def _json_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"
    ).encode()


def _sha256(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def _atomic_write(path: Path, content: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as handle:
        handle.write(content)
        handle.flush()
        os.fsync(handle.fileno())
        temporary = Path(handle.name)
    os.replace(temporary, path)


def _provenance_note(event_modes: dict[str, str]) -> tuple[str, str]:
    modes = set(event_modes.values())
    supported = {"official_fis_startlist", "latest_season_fallback"}
    if not modes or not modes <= supported:
        raise WeekendPublicationError(f"unsupported event provenance: {sorted(modes)}")
    if "latest_season_fallback" in modes:
        return (
            "provisional",
            (
                "Official startlists were not available for every race. Participation "
                "uses validated latest-season fallback pools and may change before race day."
            ),
        )
    return "official", "Participation is based on official FIS startlists."


def _schedule_date(value: str) -> date:
    try:
        return datetime.strptime(value, "%m/%d/%Y").date()
    except ValueError as exc:
        raise WeekendPublicationError(
            f"invalid weekend schedule date: {value!r}"
        ) from exc


def resolve_active_weekend_group_date(*, schedule_path: Path, as_of: date) -> date:
    """Return the stable group date while any normal race in that group is active."""
    try:
        with schedule_path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            fields = set(reader.fieldnames or ())
            required = {"Date", "Race_Date", "Distance", "Stage"}
            if missing := sorted(required - fields):
                raise WeekendPublicationError(
                    f"weekend schedule is missing columns: {missing}"
                )
            groups: dict[date, list[date]] = {}
            for row in reader:
                if row["Distance"] in {"Rel", "Ts"} or row["Stage"] == "1":
                    continue
                group_date = _schedule_date(row["Date"])
                groups.setdefault(group_date, []).append(
                    _schedule_date(row["Race_Date"])
                )
    except OSError as exc:
        raise WeekendPublicationError(f"cannot read weekend schedule: {exc}") from exc
    active = sorted(
        group_date
        for group_date, race_dates in groups.items()
        if group_date <= as_of <= max(race_dates)
    )
    if len(active) > 1:
        raise WeekendPublicationError(
            f"multiple active weekend groups for {as_of.isoformat()}: {active}"
        )
    return active[0] if active else as_of


def _validated_candidate_outputs(
    candidate_root: Path, logical_date: date
) -> tuple[dict[str, Any], dict[Path, bytes]]:
    candidate_report = _read_json(
        candidate_root / "publication_candidate_report.json", "candidate report"
    )
    if candidate_report.get("schema_version") != CANDIDATE_SCHEMA:
        raise WeekendPublicationError("unsupported candidate report schema")
    if candidate_report.get("status") != "candidate":
        raise WeekendPublicationError("publication candidate is not ready")
    if candidate_report.get("logical_date") != logical_date.isoformat():
        raise WeekendPublicationError("candidate logical date does not match")
    if candidate_report.get("publication") != {"status": "disabled"}:
        raise WeekendPublicationError("candidate publication must remain disabled")
    outputs = candidate_report.get("outputs")
    if not isinstance(outputs, dict) or not outputs:
        raise WeekendPublicationError("candidate report has no outputs")
    documents: dict[Path, bytes] = {}
    for relative, metadata in outputs.items():
        path = Path(relative)
        if path.is_absolute() or ".." in path.parts:
            raise WeekendPublicationError(f"unsafe candidate output path: {relative}")
        try:
            content = (candidate_root / path).read_bytes()
        except OSError as exc:
            raise WeekendPublicationError(
                f"cannot read candidate output: {path}"
            ) from exc
        if not isinstance(metadata, dict) or metadata.get("sha256") != _sha256(content):
            raise WeekendPublicationError(f"candidate checksum mismatch: {relative}")
        documents[path] = content
    return candidate_report, documents


def _fantasy_candidate_documents(
    *,
    report_path: Path | None,
    teams_path: Path | None,
    logical_date: date,
    date_key: str,
) -> tuple[dict[str, Any], dict[Path, bytes], list[str], int]:
    if report_path is None and teams_path is None:
        return {"status": "not_requested", "included": False}, {}, [], 0
    if report_path is None or teams_path is None:
        raise WeekendPublicationError(
            "fantasy report and teams paths must be supplied together"
        )

    report = _read_json(report_path, "fantasy optimizer report")
    logical_date_text = logical_date.isoformat()
    if report.get("schema_version") != FANTASY_REPORT_SCHEMA:
        raise WeekendPublicationError("unsupported fantasy optimizer report schema")
    if report.get("logical_date") != logical_date_text:
        raise WeekendPublicationError("fantasy optimizer logical date does not match")
    if report.get("publication") != {"status": "disabled"}:
        raise WeekendPublicationError(
            "fantasy optimizer publication must remain disabled"
        )
    if report.get("weekend_publication_blocked") is not False:
        raise WeekendPublicationError(
            "fantasy report may not block weekend publication"
        )

    status = report.get("status")
    if status in {"skipped", "blocked"}:
        return (
            {
                "status": status,
                "included": False,
                "reason": str(report.get("reason", "fantasy candidate unavailable")),
            },
            {},
            [],
            0,
        )
    if status != "candidate":
        raise WeekendPublicationError(
            f"unsupported fantasy optimizer status: {status!r}"
        )

    try:
        teams_bytes = teams_path.read_bytes()
    except OSError as exc:
        raise WeekendPublicationError(f"cannot read fantasy teams: {exc}") from exc
    output = report.get("outputs", {}).get(teams_path.name, {})
    if output.get("sha256") != _sha256(teams_bytes):
        raise WeekendPublicationError("fantasy teams checksum does not match report")
    if output.get("schema_version") != FANTASY_TEAM_SCHEMA or output.get(
        "teams"
    ) != len(OBJECTIVES):
        raise WeekendPublicationError("fantasy teams report metadata is invalid")
    teams = _read_json(teams_path, "fantasy teams")
    if teams.get("schema_version") != FANTASY_TEAM_SCHEMA:
        raise WeekendPublicationError("unsupported fantasy teams schema")
    if teams.get("logical_date") != logical_date_text:
        raise WeekendPublicationError("fantasy teams logical date does not match")
    if teams.get("publication") != {"status": "disabled"}:
        raise WeekendPublicationError("fantasy teams publication must remain disabled")
    team_documents = teams.get("teams")
    if not isinstance(team_documents, dict) or set(team_documents) != set(OBJECTIVES):
        raise WeekendPublicationError("fantasy teams must contain all three objectives")

    documents: dict[Path, bytes] = {}
    sections = ["## Fantasy XC Teams", ""]
    total_rows = 0
    for objective in OBJECTIVES:
        team = team_documents[objective]
        if not isinstance(team, dict) or not isinstance(team.get("rows"), list):
            raise WeekendPublicationError(f"invalid {objective} fantasy team")
        required_row_fields = {
            "fis_code",
            "skier",
            "nation",
            "sex",
            "price",
            "objective_points",
        }
        if any(
            not isinstance(row, dict) or not required_row_fields <= row.keys()
            for row in team["rows"]
        ):
            raise WeekendPublicationError(
                f"invalid {objective} fantasy team display fields"
            )
        try:
            validation = validate_team(objective=objective, team_rows=team["rows"])
        except (TypeError, ValueError) as exc:
            raise WeekendPublicationError(
                f"invalid {objective} fantasy team: {exc}"
            ) from exc
        if validation.get("status") != "valid":
            raise WeekendPublicationError(f"invalid {objective} fantasy team")
        if (
            team.get("objective") != objective
            or team.get("total_price") != validation["total_price"]
            or team.get("validation") != validation
        ):
            raise WeekendPublicationError(
                f"{objective} fantasy team metadata does not match its rows"
            )
        rows = [
            [
                row["skier"],
                row["nation"],
                row["sex"],
                row["price"],
                row["objective_points"],
            ]
            for row in team["rows"]
        ]
        total_rows += len(rows)
        filename = f"fantasy-{objective}.json"
        stem = filename.removesuffix(".json")
        sections.extend(
            (
                f"### {FANTASY_LABELS[objective]}",
                "",
                f"**Budget used:** {validation['total_price']:,} / {validation['budget']:,}",
                "",
                f'{{{{< cross-country/datatable "cross-country/weekly-picks/{date_key}/{stem}" >}}}}',
                "",
            )
        )
        document = {
            "objective": objective,
            "headers": list(FANTASY_HEADERS),
            "rows": rows,
        }
        relative = Path("data/cross-country/weekly-picks") / date_key / filename
        documents[relative] = _json_bytes(document)
    return (
        {
            "status": "candidate",
            "included": True,
            "team_count": len(OBJECTIVES),
            "row_count": total_rows,
        },
        documents,
        sections,
        total_rows,
    )


def build_weekend_candidate(
    *,
    workbook_path: Path,
    runtime_report_path: Path,
    validation_report_path: Path,
    logical_date: date,
    output_root: Path,
    fantasy_report_path: Path | None = None,
    fantasy_teams_path: Path | None = None,
) -> dict[str, Any]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise WeekendPublicationError(
            "openpyxl is required to build the candidate"
        ) from exc

    runtime = _read_json(runtime_report_path, "runtime report")
    validation = _read_json(validation_report_path, "validation report")
    logical_date_text = logical_date.isoformat()
    if runtime.get("schema_version") != RUNTIME_SCHEMA:
        raise WeekendPublicationError("unsupported runtime report schema")
    if validation.get("schema_version") != VALIDATION_SCHEMA:
        raise WeekendPublicationError("unsupported validation report schema")
    for label, report in (("runtime", runtime), ("validation", validation)):
        if report.get("logical_date") != logical_date_text:
            raise WeekendPublicationError(f"{label} logical date does not match")
        if report.get("publication") != {"status": "disabled"}:
            raise WeekendPublicationError(f"{label} publication must remain disabled")
    if validation.get("status") != "passed":
        raise WeekendPublicationError("shadow validation did not pass")
    expected_workbook_sha = (
        validation.get("outputs", {})
        .get("weekend_top_30_predicted_points.xlsx", {})
        .get("sha256")
    )
    workbook_bytes = workbook_path.read_bytes()
    if expected_workbook_sha != _sha256(workbook_bytes):
        raise WeekendPublicationError("workbook checksum does not match validation")

    try:
        workbook = load_workbook(workbook_path, read_only=True, data_only=True)
    except (OSError, ValueError) as exc:
        raise WeekendPublicationError(
            f"cannot read prediction workbook: {exc}"
        ) from exc
    if set(workbook.sheetnames) != set(validation.get("sheets", {})):
        raise WeekendPublicationError("workbook sheets do not match validation")

    date_key = logical_date.strftime("%Y%m%d")
    documents: dict[Path, bytes] = {}
    sections: list[str] = []
    total_rows = 0
    for sheet_name in workbook.sheetnames:
        if sheet_name not in SHEET_OUTPUTS:
            raise WeekendPublicationError(f"unsupported workbook sheet: {sheet_name}")
        filename, expected_sex = SHEET_OUTPUTS[sheet_name]
        rows = workbook[sheet_name].iter_rows(values_only=True)
        headers = tuple(next(rows, ()))
        if headers != EXPECTED_HEADERS:
            raise WeekendPublicationError(f"unexpected headers in {sheet_name}")
        data_rows = [list(row) for row in rows]
        if not data_rows or any(row[3] != expected_sex for row in data_rows):
            raise WeekendPublicationError(f"invalid rows in {sheet_name}")
        total_rows += len(data_rows)
        gender = "Men" if expected_sex == "M" else "Women"
        stem = filename.removesuffix(".json")
        sections.extend(
            (
                f"## {gender}",
                "",
                f'{{{{< cross-country/datatable "cross-country/weekly-picks/{date_key}/{stem}" >}}}}',
                "",
            )
        )
        document = {
            "sheet_name": sheet_name,
            "headers": list(headers),
            "rows": data_rows,
        }
        documents[Path("data/cross-country/weekly-picks") / date_key / filename] = (
            _json_bytes(document)
        )
    workbook.close()

    fantasy, fantasy_documents, fantasy_sections, fantasy_rows = (
        _fantasy_candidate_documents(
            report_path=fantasy_report_path,
            teams_path=fantasy_teams_path,
            logical_date=logical_date,
            date_key=date_key,
        )
    )
    documents.update(fantasy_documents)
    sections.extend(fantasy_sections)

    source_state, note = _provenance_note(runtime.get("event_modes", {}))
    display_date = logical_date.strftime("%m/%d/%Y")
    post = "\n".join(
        (
            "---",
            f'title: "Cross-Country Weekend Picks for {display_date}"',
            f"date: {logical_date_text}T00:00:00+00:00",
            "draft: false",
            f'weekend_source_state: "{source_state}"',
            'tags: ["predictions", "cross-country", "weekend-picks"]',
            "---",
            "",
            "*See [Race Picks Methodology](/post/methods/race-picks/) for details.*",
            "",
            f"> {note}",
            "",
            *sections,
        )
    ).encode()
    documents[Path("content/post/weekly-picks") / f"{date_key}.md"] = post

    output_root = output_root.expanduser().resolve()
    existing_report = output_root / "publication_candidate_report.json"
    if output_root.exists() and any(output_root.iterdir()):
        existing = _read_json(existing_report, "existing candidate report")
        if existing.get("schema_version") != CANDIDATE_SCHEMA:
            raise WeekendPublicationError(
                f"refusing to write into unrecognized output root: {output_root}"
            )
    for objective in OBJECTIVES:
        optional = (
            Path("data/cross-country/weekly-picks")
            / date_key
            / f"fantasy-{objective}.json"
        )
        if optional not in documents:
            (output_root / optional).unlink(missing_ok=True)
    output_metadata = {
        str(path): {
            "sha256": _sha256(content),
            **(
                {"rows": len(json.loads(content)["rows"])}
                if path.suffix == ".json"
                else {}
            ),
        }
        for path, content in sorted(documents.items(), key=lambda item: str(item[0]))
    }
    report = {
        "schema_version": CANDIDATE_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date_text,
        "source_state": source_state,
        "fantasy": fantasy,
        "publication": {"status": "disabled"},
        "row_count": total_rows + fantasy_rows,
        "outputs": output_metadata,
    }
    for path, content in documents.items():
        _atomic_write(output_root / path, content)
    _atomic_write(existing_report, _json_bytes(report))
    return report


def render_weekend_candidate(
    *,
    blog_root: Path,
    candidate_root: Path,
    logical_date: date,
    destination: Path,
    report_path: Path,
) -> dict[str, Any]:
    hugo = shutil.which("hugo")
    if not hugo:
        raise WeekendPublicationError("hugo is required to render the candidate")
    blog_root = blog_root.expanduser().resolve()
    candidate_root = candidate_root.expanduser().resolve()
    _validated_candidate_outputs(candidate_root, logical_date)

    destination = destination.expanduser().resolve()
    destination.parent.mkdir(parents=True, exist_ok=True)
    date_key = logical_date.strftime("%Y%m%d")
    with tempfile.TemporaryDirectory(prefix="xc-weekend-hugo-") as directory:
        temporary = Path(directory)
        static_root = temporary / "static"
        static_root.mkdir()
        config = temporary / "hugo.toml"
        config.write_text(
            'baseURL = "https://test.invalid/"\n'
            'title = "Nordic Numbers Cross-Country Weekend Shadow"\n'
            'theme = "white"\n'
            'languageCode = "en-us"\n'
            f'contentDir = "{(candidate_root / "content").as_posix()}"\n'
            f'dataDir = "{(candidate_root / "data").as_posix()}"\n'
            f'staticDir = "{static_root.as_posix()}"\n'
            'disableKinds = ["home", "section", "taxonomy", "term", "RSS", '
            '"sitemap", "robotsTXT", "404"]\n\n'
            "[params]\n"
            '  header_image = ""\n'
            '  description = "Deterministic cross-country weekend shadow"\n',
            encoding="utf-8",
        )
        completed = subprocess.run(
            (
                hugo,
                "--source",
                str(blog_root),
                "--config",
                str(config),
                "--destination",
                str(destination),
                "--cleanDestinationDir",
                "--noBuildLock",
                "--buildFuture",
                "--panicOnWarning",
                "--logLevel",
                "warn",
            ),
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
    if completed.returncode != 0:
        raise WeekendPublicationError(
            "cross-country weekend Hugo render failed:\n" + completed.stdout.strip()
        )
    page_relative = Path("post/weekly-picks") / date_key / "index.html"
    page = destination / page_relative
    if not page.is_file():
        raise WeekendPublicationError(f"expected rendered page is missing: {page}")
    html = page.read_text(encoding="utf-8")
    json_roots = candidate_root / "data/cross-country/weekly-picks" / date_key
    expected_markers = [
        f"Cross-Country Weekend Picks for {logical_date.strftime('%m/%d/%Y')}",
        "table-cross-countryweekly-picks" + date_key + "men",
        "table-cross-countryweekly-picks" + date_key + "ladies",
    ]
    for filename in ("men.json", "ladies.json"):
        document = _read_json(json_roots / filename, filename)
        try:
            expected_markers.append(str(document["rows"][0][0]))
        except (KeyError, IndexError, TypeError) as exc:
            raise WeekendPublicationError(
                f"candidate data is empty: {filename}"
            ) from exc
    candidate_report = _read_json(
        candidate_root / "publication_candidate_report.json", "candidate report"
    )
    if candidate_report.get("fantasy", {}).get("included") is True:
        for objective in OBJECTIVES:
            filename = f"fantasy-{objective}.json"
            document = _read_json(json_roots / filename, filename)
            expected_markers.extend(
                (
                    "table-cross-countryweekly-picks"
                    + date_key
                    + f"fantasy-{objective}",
                    str(document["rows"][0][0]),
                )
            )
    missing = [marker for marker in expected_markers if marker not in html]
    if missing:
        raise WeekendPublicationError(
            f"rendered candidate is missing expected markers: {missing}"
        )
    report = {
        "schema_version": RENDER_SCHEMA,
        "status": "passed",
        "logical_date": logical_date.isoformat(),
        "publication": {"status": "disabled"},
        "page": str(page_relative),
        "bytes": page.stat().st_size,
        "sha256": _sha256(page.read_bytes()),
    }
    _atomic_write(report_path.expanduser().resolve(), _json_bytes(report))
    return report


def install_weekend_candidate(
    *,
    candidate_root: Path,
    render_report_path: Path,
    rendered_site_root: Path,
    logical_date: date,
    repository_root: Path,
) -> list[Path]:
    candidate_root = candidate_root.expanduser().resolve()
    candidate_report, documents = _validated_candidate_outputs(
        candidate_root, logical_date
    )
    date_key = logical_date.strftime("%Y%m%d")
    expected = {
        Path("content/post/weekly-picks") / f"{date_key}.md",
        Path("data/cross-country/weekly-picks") / date_key / "men.json",
        Path("data/cross-country/weekly-picks") / date_key / "ladies.json",
    }
    fantasy = candidate_report.get("fantasy", {})
    if fantasy.get("included") is True:
        expected.update(
            Path("data/cross-country/weekly-picks")
            / date_key
            / f"fantasy-{objective}.json"
            for objective in OBJECTIVES
        )
    elif fantasy.get("status") not in {"not_requested", "skipped", "blocked"}:
        raise WeekendPublicationError("candidate fantasy state is invalid")
    if set(documents) != expected:
        raise WeekendPublicationError(
            f"candidate output set does not match publication contract: {sorted(documents)}"
        )
    render = _read_json(render_report_path, "render report")
    page_relative = Path("post/weekly-picks") / date_key / "index.html"
    if (
        render.get("schema_version") != RENDER_SCHEMA
        or render.get("status") != "passed"
        or render.get("logical_date") != logical_date.isoformat()
        or render.get("publication") != {"status": "disabled"}
        or render.get("page") != str(page_relative)
    ):
        raise WeekendPublicationError("render report is not eligible for publication")
    page = rendered_site_root.expanduser().resolve() / page_relative
    try:
        page_content = page.read_bytes()
    except OSError as exc:
        raise WeekendPublicationError(
            f"cannot read rendered candidate: {page}"
        ) from exc
    if render.get("sha256") != _sha256(page_content):
        raise WeekendPublicationError(
            "rendered candidate checksum does not match report"
        )

    repository_root = repository_root.expanduser().resolve()
    if not (repository_root / ".git").exists():
        raise WeekendPublicationError("publication target is not a Git checkout")
    post_relative = Path("content/post/weekly-picks") / f"{date_key}.md"
    existing_post = repository_root / post_relative
    if (
        candidate_report.get("source_state") == "provisional"
        and existing_post.is_file()
        and 'weekend_source_state: "official"'
        in existing_post.read_text(encoding="utf-8")
    ):
        raise WeekendPublicationError(
            "refusing to replace an official weekend post with provisional inputs"
        )
    installed: list[Path] = []
    for objective in OBJECTIVES:
        optional = (
            Path("data/cross-country/weekly-picks")
            / date_key
            / f"fantasy-{objective}.json"
        )
        if optional not in expected:
            (repository_root / optional).unlink(missing_ok=True)
    for relative, content in sorted(documents.items(), key=lambda item: str(item[0])):
        destination = repository_root / relative
        _atomic_write(destination, content)
        installed.append(relative)
    return installed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    build = subparsers.add_parser("build")
    build.add_argument("--workbook", required=True, type=Path)
    build.add_argument("--runtime-report", required=True, type=Path)
    build.add_argument("--validation-report", required=True, type=Path)
    build.add_argument("--logical-date", required=True, type=date.fromisoformat)
    build.add_argument("--output-root", required=True, type=Path)
    build.add_argument("--fantasy-report", type=Path)
    build.add_argument("--fantasy-teams", type=Path)
    render = subparsers.add_parser("render")
    render.add_argument("--blog-root", required=True, type=Path)
    render.add_argument("--candidate-root", required=True, type=Path)
    render.add_argument("--logical-date", required=True, type=date.fromisoformat)
    render.add_argument("--destination", required=True, type=Path)
    render.add_argument("--report", required=True, type=Path)
    resolve_date = subparsers.add_parser("resolve-date")
    resolve_date.add_argument("--schedule", required=True, type=Path)
    resolve_date.add_argument("--as-of", required=True, type=date.fromisoformat)
    install = subparsers.add_parser("install")
    install.add_argument("--candidate-root", required=True, type=Path)
    install.add_argument("--render-report", required=True, type=Path)
    install.add_argument("--rendered-site-root", required=True, type=Path)
    install.add_argument("--logical-date", required=True, type=date.fromisoformat)
    install.add_argument("--repository-root", required=True, type=Path)
    args = parser.parse_args()
    if args.command == "build":
        report = build_weekend_candidate(
            workbook_path=args.workbook,
            runtime_report_path=args.runtime_report,
            validation_report_path=args.validation_report,
            logical_date=args.logical_date,
            output_root=args.output_root,
            fantasy_report_path=args.fantasy_report,
            fantasy_teams_path=args.fantasy_teams,
        )
    elif args.command == "render":
        report = render_weekend_candidate(
            blog_root=args.blog_root,
            candidate_root=args.candidate_root,
            logical_date=args.logical_date,
            destination=args.destination,
            report_path=args.report,
        )
    elif args.command == "resolve-date":
        print(
            resolve_active_weekend_group_date(
                schedule_path=args.schedule, as_of=args.as_of
            ).isoformat()
        )
        return 0
    else:
        installed = install_weekend_candidate(
            candidate_root=args.candidate_root,
            render_report_path=args.render_report,
            rendered_site_root=args.rendered_site_root,
            logical_date=args.logical_date,
            repository_root=args.repository_root,
        )
        report = {"installed": [str(path) for path in installed]}
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
