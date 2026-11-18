"""Prepare and publish validated cross-country individual predictions."""

from __future__ import annotations

import argparse
import csv
import json
import os
import shutil
import tempfile
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path

TEAM_DISTANCES = {"Rel", "Ts"}
GENDERS = {"M": "men", "L": "ladies"}
PROBABILITY_COLUMNS = (
    "Win",
    "Podium",
    "Top-5",
    "Final",
    "Top-10",
    "Semifinal",
    "Top-30",
    "Quarterfinal",
)


class IndividualPublicationError(RuntimeError):
    """Raised when an individual prediction artifact is unsafe to publish."""


@dataclass(frozen=True, order=True)
class IndividualEvent:
    gender: str
    distance: str
    technique: str


def _parse_schedule_date(value: str) -> date:
    for format_string in ("%m/%d/%Y", "%Y-%m-%d"):
        try:
            return datetime.strptime(value.strip(), format_string).date()
        except ValueError:
            continue
    raise IndividualPublicationError(f"unsupported schedule date: {value!r}")


def _read_schedule(schedule: Path) -> tuple[list[str], list[dict[str, str]]]:
    try:
        with schedule.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            fieldnames = reader.fieldnames
            rows = list(reader)
    except OSError as exc:
        raise IndividualPublicationError(
            f"cannot read cross-country schedule: {exc}"
        ) from exc
    if not fieldnames:
        raise IndividualPublicationError("cross-country schedule has no header")
    return fieldnames, rows


def _event_rows(schedule: Path, logical_date: date) -> list[dict[str, str]]:
    _, rows = _read_schedule(schedule)
    return [
        row
        for row in rows
        if _parse_schedule_date(row.get("Date", "")) == logical_date
        and row.get("Sex", "").strip() in GENDERS
        and row.get("Distance", "").strip() not in TEAM_DISTANCES
    ]


def detect_individual_events(
    schedule: Path, logical_date: date
) -> tuple[IndividualEvent, ...]:
    events = {
        IndividualEvent(
            gender=GENDERS[row["Sex"].strip()],
            distance=row.get("Distance", "").strip(),
            technique=row.get("Technique", "").strip(),
        )
        for row in _event_rows(schedule, logical_date)
    }
    return tuple(sorted(events))


def _atomic_csv(path: Path, fieldnames: list[str], rows: list[dict[str, str]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w", newline="", encoding="utf-8", dir=path.parent, delete=False
    ) as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)
        temporary = Path(handle.name)
    temporary.replace(path)


def _materialize_elevation_history(
    history: Path, elevations_path: Path, output: Path
) -> None:
    try:
        with elevations_path.open(newline="", encoding="utf-8-sig") as handle:
            elevations = {
                row["City"].strip().casefold(): row["Elevation"].strip()
                for row in csv.DictReader(handle)
                if row.get("City", "").strip()
            }
        with history.open(newline="", encoding="utf-8-sig") as source:
            reader = csv.DictReader(source)
            if not reader.fieldnames:
                raise IndividualPublicationError(f"history has no header: {history}")
            fieldnames = [*reader.fieldnames]
            if "Elevation" not in fieldnames:
                fieldnames.append("Elevation")
            output.parent.mkdir(parents=True, exist_ok=True)
            with tempfile.NamedTemporaryFile(
                mode="w",
                newline="",
                encoding="utf-8",
                dir=output.parent,
                delete=False,
            ) as handle:
                writer = csv.DictWriter(handle, fieldnames=fieldnames)
                writer.writeheader()
                for row in reader:
                    row["Elevation"] = elevations.get(
                        row.get("City", "").strip().casefold(), "0"
                    )
                    writer.writerow(row)
                temporary = Path(handle.name)
    except OSError as exc:
        raise IndividualPublicationError(
            f"cannot materialize elevation history: {exc}"
        ) from exc
    temporary.replace(output)


def prepare_runtime(schedule: Path, logical_date: date, data_root: Path) -> None:
    fieldnames, _ = _read_schedule(schedule)
    rows = _event_rows(schedule, logical_date)
    if not rows:
        raise IndividualPublicationError("no individual events to prepare")
    _atomic_csv(data_root / "races.csv", fieldnames, rows)
    elevations = data_root / "elevation.csv"
    for gender in ("men", "ladies"):
        history = data_root / f"{gender}_chrono.csv"
        if not history.is_file():
            raise IndividualPublicationError(f"missing accepted history: {history}")
        shutil.copyfile(history, data_root / f"{gender}_chrono_pred.csv")
        _materialize_elevation_history(
            history, elevations, data_root / f"{gender}_chrono_elevation.csv"
        )


def _load_prediction(path: Path) -> dict:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise IndividualPublicationError(
            f"cannot read prediction artifact {path}: {exc}"
        ) from exc
    headers = document.get("headers")
    rows = document.get("rows")
    if not isinstance(headers, list) or not isinstance(rows, list) or not rows:
        raise IndividualPublicationError(f"prediction artifact has no rows: {path}")
    if any(not isinstance(row, list) or len(row) != len(headers) for row in rows):
        raise IndividualPublicationError(
            f"prediction rows do not match headers: {path}"
        )
    required = {"Skier", "ID", "Nation", "Win", "Podium"}
    missing = required - set(headers)
    if missing:
        raise IndividualPublicationError(
            f"prediction artifact is missing headers {sorted(missing)}: {path}"
        )
    indexes = [headers.index(name) for name in PROBABILITY_COLUMNS if name in headers]
    athlete_ids: set[str] = set()
    for row in rows:
        athlete_id = str(row[headers.index("ID")]).strip()
        if not athlete_id or athlete_id in athlete_ids:
            raise IndividualPublicationError(
                f"prediction artifact has blank or duplicate athlete ID: {athlete_id!r}"
            )
        athlete_ids.add(athlete_id)
        try:
            values = [float(row[index]) for index in indexes]
        except (TypeError, ValueError) as exc:
            raise IndividualPublicationError(
                f"prediction artifact has non-numeric probabilities for {athlete_id}"
            ) from exc
        if any(value < 0 or value > 100 for value in values):
            raise IndividualPublicationError(
                f"prediction artifact has out-of-range probabilities for {athlete_id}"
            )
        if values != sorted(values):
            raise IndividualPublicationError(
                f"prediction artifact has non-monotonic probabilities for {athlete_id}"
            )
    return document


def _gender_artifacts(data_root: Path, gender: str) -> tuple[Path, ...]:
    paths = tuple(sorted(data_root.glob(f"{gender}_position_probabilities*.json")))
    if not paths:
        raise IndividualPublicationError(
            f"no {gender} individual prediction artifacts were generated"
        )
    for path in paths:
        _load_prediction(path)
    return paths


def render_post(
    logical_date: date, events: tuple[IndividualEvent, ...], data_root: Path
) -> str:
    if not events:
        raise IndividualPublicationError(
            "cannot render an individual prediction post without events"
        )
    date_key = logical_date.strftime("%Y%m%d")
    expected_genders = sorted({event.gender for event in events})
    artifacts = {
        gender: _gender_artifacts(data_root, gender) for gender in expected_genders
    }
    lines = [
        "---",
        f'title: "Cross-Country Race Picks for {logical_date.strftime("%m/%d/%Y")}"',
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
    for gender in expected_genders:
        lines.extend([f"### {'Men' if gender == 'men' else 'Ladies'}", ""])
        for index, path in enumerate(artifacts[gender], start=1):
            document = _load_prediction(path)
            label = str(document.get("sheet_name") or f"Race {index}")
            if len(artifacts[gender]) > 1:
                lines.extend([f"#### {label}", ""])
            lines.extend(
                [
                    "#### Position Probabilities"
                    if len(artifacts[gender]) == 1
                    else "##### Position Probabilities",
                    "",
                    f'{{{{< cross-country/datatable "cross-country/drafts/race-picks/{date_key}/{path.stem}" >}}}}',
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
    for name in ("detect", "prepare", "build"):
        command = subparsers.add_parser(name)
        command.add_argument("--schedule", required=True, type=Path)
        command.add_argument("--logical-date", required=True, type=_logical_date)
        if name == "prepare":
            command.add_argument("--data-root", required=True, type=Path)
        if name == "build":
            command.add_argument("--data-root", required=True, type=Path)
            command.add_argument("--output", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    events = detect_individual_events(args.schedule, args.logical_date)
    if args.command == "detect":
        values = {
            "has_individual_event": str(bool(events)).lower(),
            "date_key": args.logical_date.strftime("%Y%m%d"),
            "events": ",".join(
                f"{event.gender}:{event.distance}:{event.technique}" for event in events
            ),
        }
        _write_github_output(values)
        print(json.dumps(values, sort_keys=True))
        return 0
    if args.command == "prepare":
        prepare_runtime(args.schedule, args.logical_date, args.data_root)
        return 0
    rendered = render_post(args.logical_date, events, args.data_root)
    write_post(args.output, rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
