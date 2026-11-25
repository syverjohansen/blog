"""Detect, validate, and render hosted Nordic Numbers weekly recaps."""

from __future__ import annotations

import argparse
import csv
import json
import os
import tempfile
from dataclasses import dataclass
from datetime import date, datetime, timedelta
from pathlib import Path

SUPPORTED_SPORTS = {
    "alpine": "alpine",
    "cross-country": "ski",
    "skijump": "skijump",
}
GENDERS = ("men", "ladies")
ARTIFACT_TYPES = ("elo_change", "standings_predictions", "magic_numbers")


class WeeklyRecapError(RuntimeError):
    """Raised when weekly recap inputs or outputs are unsafe to publish."""


@dataclass(frozen=True, order=True)
class RecapSport:
    content_name: str
    ski_name: str


def _parse_schedule_date(value: str) -> date:
    for format_string in ("%m/%d/%Y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), format_string).date()
        except ValueError:
            continue
    raise WeeklyRecapError(f"unsupported schedule date: {value!r}")


def _has_recent_race(schedule: Path, logical_date: date) -> bool:
    start = logical_date - timedelta(days=7)
    end = logical_date - timedelta(days=1)
    try:
        with schedule.open(newline="", encoding="utf-8-sig") as handle:
            rows = list(csv.DictReader(handle))
    except OSError as exc:
        raise WeeklyRecapError(f"cannot read schedule {schedule}: {exc}") from exc
    return any(
        start <= _parse_schedule_date(row.get("Date", "")) <= end for row in rows
    )


def detect_recap_sports(ski_root: Path, logical_date: date) -> tuple[RecapSport, ...]:
    if logical_date.weekday() != 0:
        return ()
    eligible = []
    for content_name, ski_name in SUPPORTED_SPORTS.items():
        schedule = ski_root / ski_name / "polars" / "excel365" / "races.csv"
        if _has_recent_race(schedule, logical_date):
            eligible.append(RecapSport(content_name, ski_name))
    return tuple(sorted(eligible))


def prepare_elevation_history(data_root: Path) -> None:
    elevations_path = data_root / "elevation.csv"
    try:
        with elevations_path.open(newline="", encoding="utf-8-sig") as handle:
            elevations = {
                row["City"].strip().casefold(): row["Elevation"].strip()
                for row in csv.DictReader(handle)
                if row.get("City", "").strip()
            }
        for gender in GENDERS:
            source = data_root / f"{gender}_chrono.csv"
            output = data_root / f"{gender}_chrono_elevation.csv"
            with source.open(newline="", encoding="utf-8-sig") as source_handle:
                reader = csv.DictReader(source_handle)
                if not reader.fieldnames:
                    raise WeeklyRecapError(f"history has no header: {source}")
                fieldnames = [*reader.fieldnames]
                if "Elevation" not in fieldnames:
                    fieldnames.append("Elevation")
                with tempfile.NamedTemporaryFile(
                    mode="w",
                    newline="",
                    encoding="utf-8",
                    dir=data_root,
                    delete=False,
                ) as output_handle:
                    writer = csv.DictWriter(output_handle, fieldnames=fieldnames)
                    writer.writeheader()
                    for row in reader:
                        row["Elevation"] = elevations.get(
                            row.get("City", "").strip().casefold(), "0"
                        )
                        writer.writerow(row)
                    temporary = Path(output_handle.name)
            temporary.replace(output)
    except OSError as exc:
        raise WeeklyRecapError(
            f"cannot prepare elevation history: {exc}"
        ) from exc


def _load_artifact(path: Path, *, allow_empty: bool = False) -> dict:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise WeeklyRecapError(f"cannot read recap artifact {path}: {exc}") from exc
    headers = document.get("headers")
    rows = document.get("rows")
    if not isinstance(headers, list) or not headers:
        raise WeeklyRecapError(f"recap artifact has no headers: {path}")
    if not isinstance(rows, list) or (not rows and not allow_empty):
        raise WeeklyRecapError(f"recap artifact has no rows: {path}")
    if any(not isinstance(row, list) or len(row) != len(headers) for row in rows):
        raise WeeklyRecapError(f"recap rows do not match headers: {path}")
    return document


def validate_sport_artifacts(data_root: Path, sport: str) -> tuple[Path, ...]:
    sport_root = data_root / sport
    expected = tuple(
        sport_root / f"{gender}_{artifact_type}.json"
        for gender in GENDERS
        for artifact_type in ARTIFACT_TYPES
    )
    for path in expected:
        _load_artifact(path, allow_empty=path.name.endswith("_elo_change.json"))
    return expected


def render_post(logical_date: date, sports: tuple[str, ...], data_root: Path) -> str:
    if logical_date.weekday() != 0:
        raise WeeklyRecapError("weekly recap date must be a Monday")
    if not sports:
        raise WeeklyRecapError("cannot render a weekly recap without sports")
    unknown = set(sports) - set(SUPPORTED_SPORTS)
    if unknown:
        raise WeeklyRecapError(f"unsupported recap sports: {sorted(unknown)}")
    for sport in sports:
        validate_sport_artifacts(data_root, sport)

    start = logical_date - timedelta(days=7)
    end = logical_date - timedelta(days=1)
    date_key = logical_date.strftime("%Y%m%d")
    lines = [
        "---",
        f'title: "Weekly Recap for week of {start.strftime("%m/%d/%Y")} to {end.strftime("%m/%d/%Y")}"',
        f"date: {logical_date.isoformat()}T02:00:00+00:00",
        "draft: false",
        'tags: ["recap", "skiing", "weekly-recap", "standings", "elo"]',
        "---",
        "",
    ]
    labels = {
        "alpine": "Alpine",
        "cross-country": "Cross Country",
        "skijump": "Ski Jumping",
    }
    headings = {
        "elo_change": "Elo Change",
        "standings_predictions": "Standings Predictions",
        "magic_numbers": "Magic Numbers",
    }
    for sport in sports:
        lines.extend([f"## {labels[sport]}", ""])
        for gender in GENDERS:
            lines.extend([f"### {'Men' if gender == 'men' else 'Ladies'}", ""])
            for artifact_type in ARTIFACT_TYPES:
                lines.extend(
                    [
                        f"#### {headings[artifact_type]}",
                        "",
                        f'{{{{< {sport}/datatable "{sport}/drafts/weekly-recap/{date_key}/{gender}_{artifact_type}" >}}}}',
                        "",
                    ]
                )
    return "\n".join(lines)


def write_post(output: Path, rendered: str) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", dir=output.parent, delete=False
    ) as handle:
        handle.write(rendered)
        temporary = Path(handle.name)
    temporary.replace(output)


def _logical_date(value: str) -> date:
    try:
        return date.fromisoformat(value)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("logical date must be YYYY-MM-DD") from exc


def _write_github_output(values: dict[str, str]) -> None:
    output = os.getenv("GITHUB_OUTPUT")
    if not output:
        return
    with Path(output).open("a", encoding="utf-8") as handle:
        handle.writelines(f"{key}={value}\n" for key, value in values.items())


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    detect = subparsers.add_parser("detect")
    detect.add_argument("--ski-root", required=True, type=Path)
    detect.add_argument("--logical-date", required=True, type=_logical_date)
    prepare = subparsers.add_parser("prepare-elevation")
    prepare.add_argument("--data-root", required=True, type=Path)
    build = subparsers.add_parser("build")
    build.add_argument("--logical-date", required=True, type=_logical_date)
    build.add_argument("--sports", required=True)
    build.add_argument("--data-root", required=True, type=Path)
    build.add_argument("--output", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    if args.command == "detect":
        sports = detect_recap_sports(args.ski_root, args.logical_date)
        values = {
            "should_run": str(bool(sports)).lower(),
            "date_key": args.logical_date.strftime("%Y%m%d"),
            "sports": ",".join(sport.content_name for sport in sports),
        }
        _write_github_output(values)
        print(json.dumps(values, sort_keys=True))
        return 0
    if args.command == "prepare-elevation":
        prepare_elevation_history(args.data_root)
        return 0
    sports = tuple(sport for sport in args.sports.split(",") if sport)
    rendered = render_post(args.logical_date, sports, args.data_root)
    write_post(args.output, rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
