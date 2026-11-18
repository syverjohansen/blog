"""Detect and publish validated cross-country team-event predictions."""

from __future__ import annotations

import argparse
import csv
import json
import os
import tempfile
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path

EVENT_TYPES = {"Rel": "relay", "Ts": "team_sprint"}
GENDERS = {"M": "men", "L": "ladies"}
PROBABILITY_HEADERS = ("Team", "Win", "Podium", "Top 5", "Top 10")


class TeamPublicationError(RuntimeError):
    """Raised when a team prediction artifact is unsafe to publish."""


@dataclass(frozen=True, order=True)
class TeamEvent:
    event_type: str
    gender: str

    @property
    def artifact_prefix(self) -> str:
        return f"{self.gender}_{self.event_type}"

    @property
    def title(self) -> str:
        event = "Relay" if self.event_type == "relay" else "Team Sprint"
        gender = "Men's" if self.gender == "men" else "Women's"
        return f"{gender} {event}"


def _parse_schedule_date(value: str) -> date:
    for format_string in ("%m/%d/%Y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), format_string).date()
        except ValueError:
            continue
    raise TeamPublicationError(f"unsupported schedule date: {value!r}")


def detect_team_events(schedule: Path, logical_date: date) -> tuple[TeamEvent, ...]:
    try:
        with schedule.open(newline="", encoding="utf-8-sig") as handle:
            rows = list(csv.DictReader(handle))
    except OSError as exc:
        raise TeamPublicationError(f"cannot read cross-country schedule: {exc}") from exc
    events: set[TeamEvent] = set()
    for row in rows:
        if _parse_schedule_date(row.get("Date", "")) != logical_date:
            continue
        event_type = EVENT_TYPES.get(row.get("Distance", "").strip())
        gender = GENDERS.get(row.get("Sex", "").strip())
        if event_type and gender:
            events.add(TeamEvent(event_type=event_type, gender=gender))
    return tuple(sorted(events))


def materialize_event_inputs(
    schedule: Path, logical_date: date, output_root: Path
) -> tuple[Path, ...]:
    try:
        with schedule.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            fieldnames = reader.fieldnames
            rows = list(reader)
    except OSError as exc:
        raise TeamPublicationError(f"cannot read cross-country schedule: {exc}") from exc
    if not fieldnames:
        raise TeamPublicationError("cross-country schedule has no header")
    grouped: dict[str, list[dict[str, str]]] = {}
    for row in rows:
        if _parse_schedule_date(row.get("Date", "")) != logical_date:
            continue
        event_type = EVENT_TYPES.get(row.get("Distance", "").strip())
        gender = GENDERS.get(row.get("Sex", "").strip())
        if event_type and gender:
            grouped.setdefault(event_type, []).append(row)
    output_root.mkdir(parents=True, exist_ok=True)
    outputs: list[Path] = []
    for event_type, event_rows in sorted(grouped.items()):
        output = output_root / f"{event_type}.csv"
        with tempfile.NamedTemporaryFile(
            mode="w", newline="", encoding="utf-8", dir=output_root, delete=False
        ) as handle:
            writer = csv.DictWriter(handle, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(event_rows)
            temporary = Path(handle.name)
        temporary.replace(output)
        outputs.append(output)
    return tuple(outputs)


def _load_json(path: Path) -> dict:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise TeamPublicationError(f"cannot read prediction artifact {path}: {exc}") from exc
    if not isinstance(document, dict):
        raise TeamPublicationError(f"prediction artifact is not an object: {path}")
    headers = document.get("headers")
    rows = document.get("rows")
    if not isinstance(headers, list) or not isinstance(rows, list) or not rows:
        raise TeamPublicationError(f"prediction artifact has no tabular rows: {path}")
    if any(not isinstance(row, list) or len(row) != len(headers) for row in rows):
        raise TeamPublicationError(f"prediction rows do not match headers: {path}")
    return document


def validate_event_artifacts(data_root: Path, event: TeamEvent) -> tuple[Path, Path]:
    points = data_root / f"{event.artifact_prefix}.json"
    probabilities = data_root / f"{event.artifact_prefix}_position_probabilities.json"
    points_document = _load_json(points)
    probability_document = _load_json(probabilities)
    if "Expected Points" not in points_document["headers"] or "Team" not in points_document["headers"]:
        raise TeamPublicationError(f"points artifact has invalid headers: {points}")
    missing = set(PROBABILITY_HEADERS) - set(probability_document["headers"])
    if missing:
        raise TeamPublicationError(
            f"probability artifact is missing headers {sorted(missing)}: {probabilities}"
        )
    indexes = {name: probability_document["headers"].index(name) for name in PROBABILITY_HEADERS}
    teams: set[str] = set()
    for row in probability_document["rows"]:
        team = str(row[indexes["Team"]]).strip()
        if not team or team in teams:
            raise TeamPublicationError(f"probability artifact has blank or duplicate team: {team!r}")
        teams.add(team)
        try:
            values = [float(row[indexes[name]]) for name in PROBABILITY_HEADERS[1:]]
        except (TypeError, ValueError) as exc:
            raise TeamPublicationError(f"probability artifact has non-numeric values for {team}") from exc
        if any(value < 0 or value > 1 for value in values):
            raise TeamPublicationError(f"probability artifact has out-of-range values for {team}")
        if values != sorted(values):
            raise TeamPublicationError(f"probability artifact is not monotonic for {team}")
    return points, probabilities


def render_post(logical_date: date, events: tuple[TeamEvent, ...], data_root: Path) -> str:
    if not events:
        raise TeamPublicationError("cannot render a team prediction post without events")
    date_key = logical_date.strftime("%Y%m%d")
    lines = [
        "---",
        f'title: "Cross-Country Team Race Picks for {logical_date.strftime("%m/%d/%Y")}"',
        f"date: {logical_date.isoformat()}T06:00:00+00:00",
        "draft: false",
        'tags: ["predictions", "skiing", "race-picks", "cross-country"]',
        "---",
        "",
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details on how these predictions are generated.*",
        "",
        "## Cross-Country",
        "",
    ]
    for event in events:
        points, probabilities = validate_event_artifacts(data_root, event)
        lines.extend(
            [
                f"### {event.title}",
                "",
                "#### Points",
                "",
                f'{{{{< cross-country/datatable "cross-country/drafts/race-picks/{date_key}/{points.stem}" >}}}}',
                "",
                "#### Position Probabilities",
                "",
                f'{{{{< cross-country/datatable "cross-country/drafts/race-picks/{date_key}/{probabilities.stem}" >}}}}',
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
    path = os.getenv("GITHUB_OUTPUT")
    if not path:
        return
    with Path(path).open("a", encoding="utf-8") as handle:
        handle.writelines(f"{key}={value}\n" for key, value in values.items())


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    detect = subparsers.add_parser("detect")
    detect.add_argument("--schedule", required=True, type=Path)
    detect.add_argument("--logical-date", required=True, type=_logical_date)
    prepare = subparsers.add_parser("prepare")
    prepare.add_argument("--schedule", required=True, type=Path)
    prepare.add_argument("--logical-date", required=True, type=_logical_date)
    prepare.add_argument("--output-root", required=True, type=Path)
    build = subparsers.add_parser("build")
    build.add_argument("--schedule", required=True, type=Path)
    build.add_argument("--logical-date", required=True, type=_logical_date)
    build.add_argument("--data-root", required=True, type=Path)
    build.add_argument("--output", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    events = detect_team_events(args.schedule, args.logical_date)
    if args.command == "detect":
        values = {
            "has_team_event": str(bool(events)).lower(),
            "date_key": args.logical_date.strftime("%Y%m%d"),
            "events": ",".join(event.artifact_prefix for event in events),
        }
        _write_github_output(values)
        print(json.dumps(values, sort_keys=True))
        return 0
    if args.command == "prepare":
        outputs = materialize_event_inputs(
            args.schedule, args.logical_date, args.output_root
        )
        print(json.dumps([str(path) for path in outputs]))
        return 0
    rendered = render_post(args.logical_date, events, args.data_root)
    write_post(args.output, rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
