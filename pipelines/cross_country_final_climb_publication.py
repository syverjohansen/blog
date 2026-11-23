"""Build a publication-disabled Hugo candidate from validated Final Climb outputs."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import tempfile
from datetime import date
from pathlib import Path
from typing import Any

from pipelines.cross_country_final_climb_shadow import (
    POINT_HEADERS,
    PROBABILITY_HEADERS,
    VALIDATION_SCHEMA,
)

CANDIDATE_SCHEMA = "nordicnumbers.cross-country-final-climb-candidate/v1"
WORKBOOKS = {
    "men.xlsx": ("men.json", None, POINT_HEADERS),
    "ladies.xlsx": ("ladies.json", None, POINT_HEADERS),
    "men_position_probabilities.xlsx": (
        "men_position_probabilities.json",
        "Men Race 1",
        PROBABILITY_HEADERS,
    ),
    "ladies_position_probabilities.xlsx": (
        "ladies_position_probabilities.json",
        "Ladies Race 1",
        PROBABILITY_HEADERS,
    ),
}


class FinalClimbPublicationError(RuntimeError):
    """Validated Final Climb outputs cannot safely become a Hugo candidate."""


def _read_json(path: Path, label: str) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise FinalClimbPublicationError(f"cannot read {label}: {exc}") from exc
    if not isinstance(value, dict):
        raise FinalClimbPublicationError(f"{label} must be a JSON object")
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


def build_final_climb_candidate(
    *,
    prediction_root: Path,
    validation_report_path: Path,
    logical_date: date,
    output_root: Path,
) -> dict[str, Any]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise FinalClimbPublicationError(
            "openpyxl is required to build the candidate"
        ) from exc

    validation = _read_json(validation_report_path, "validation report")
    logical_date_text = logical_date.isoformat()
    if validation.get("schema_version") != VALIDATION_SCHEMA:
        raise FinalClimbPublicationError("unsupported validation report schema")
    if validation.get("status") != "passed":
        raise FinalClimbPublicationError("Final Climb shadow validation did not pass")
    if validation.get("logical_date") != logical_date_text:
        raise FinalClimbPublicationError("validation logical date does not match")
    if validation.get("publication") != {"status": "disabled"}:
        raise FinalClimbPublicationError("validation publication must remain disabled")
    participant_source_mode = validation.get("participant_source_mode")
    if participant_source_mode not in {
        "official_startlist",
        "historical_results_replay",
    }:
        raise FinalClimbPublicationError("invalid participant source mode")

    source_root = prediction_root.expanduser().resolve() / logical_date.strftime(
        "%Y%m%d"
    )
    validated_outputs = validation.get("outputs")
    if not isinstance(validated_outputs, dict):
        raise FinalClimbPublicationError("validation report has no outputs")
    date_key = logical_date.strftime("%Y%m%d")
    data_root = Path("data/cross-country/drafts/final-climb-picks") / date_key
    documents: dict[Path, bytes] = {}
    row_counts: dict[str, int] = {}

    for workbook_name, (
        json_name,
        expected_sheet,
        expected_headers,
    ) in WORKBOOKS.items():
        path = source_root / workbook_name
        try:
            content = path.read_bytes()
        except OSError as exc:
            raise FinalClimbPublicationError(
                f"cannot read validated workbook: {path}"
            ) from exc
        expected_sha = validated_outputs.get(workbook_name, {}).get("sha256")
        if expected_sha != _sha256(content):
            raise FinalClimbPublicationError(
                f"workbook checksum does not match validation: {workbook_name}"
            )
        try:
            workbook = load_workbook(path, read_only=True, data_only=True)
        except (OSError, ValueError) as exc:
            raise FinalClimbPublicationError(
                f"cannot open workbook: {workbook_name}"
            ) from exc
        if expected_sheet is None:
            if len(workbook.sheetnames) != 1:
                workbook.close()
                raise FinalClimbPublicationError(
                    f"unexpected sheets in {workbook_name}"
                )
            sheet = workbook.active
        else:
            if workbook.sheetnames != [expected_sheet]:
                workbook.close()
                raise FinalClimbPublicationError(
                    f"unexpected sheets in {workbook_name}"
                )
            sheet = workbook[expected_sheet]
        rows = list(sheet.iter_rows(values_only=True))
        sheet_name = sheet.title
        workbook.close()
        if not rows or tuple(rows[0]) != expected_headers:
            raise FinalClimbPublicationError(f"unexpected headers in {workbook_name}")
        data_rows = [list(row) for row in rows[1:]]
        if not data_rows:
            raise FinalClimbPublicationError(
                f"validated workbook is empty: {workbook_name}"
            )
        documents[data_root / json_name] = _json_bytes(
            {
                "sheet_name": sheet_name,
                "headers": [header.replace("-", " ") for header in expected_headers],
                "rows": data_rows,
            }
        )
        row_counts[json_name] = len(data_rows)

    source_note = (
        "This historical replay candidate uses classified FIS results and cannot "
        "be published."
        if participant_source_mode == "historical_results_replay"
        else "This candidate was generated from validated official Final Climb "
        "startlists. Publication remains disabled."
    )
    post = "\n".join(
        (
            "---",
            f'title: "Final Climb Picks for {logical_date.strftime("%m/%d/%Y")}"',
            f"date: {logical_date_text}T00:00:00+00:00",
            "draft: false",
            'final_climb_source_state: "validated-shadow"',
            f'participant_source_mode: "{participant_source_mode}"',
            'tags: ["predictions", "cross-country", "tour-de-ski", "final-climb"]',
            "---",
            "",
            "*See [Race Picks Methodology](/post/methods/race-picks/) for details.*",
            "",
            f"> {source_note}",
            "",
            "## Men's Points",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/final-climb-picks/{date_key}/men" >}}}}',
            "",
            "## Ladies' Points",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/final-climb-picks/{date_key}/ladies" >}}}}',
            "",
            "## Men's Position Probabilities",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/final-climb-picks/{date_key}/men_position_probabilities" >}}}}',
            "",
            "## Ladies' Position Probabilities",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/final-climb-picks/{date_key}/ladies_position_probabilities" >}}}}',
            "",
        )
    ).encode()
    documents[Path("content/post/final-climb-picks") / f"{date_key}.md"] = post

    output_root = output_root.expanduser().resolve()
    existing_report = output_root / "publication_candidate_report.json"
    if output_root.exists() and any(output_root.iterdir()):
        existing = _read_json(existing_report, "existing candidate report")
        if existing.get("schema_version") != CANDIDATE_SCHEMA:
            raise FinalClimbPublicationError(
                f"refusing to write into unrecognized output root: {output_root}"
            )
    output_metadata = {
        str(path): {
            "sha256": _sha256(content),
            **({"rows": row_counts[path.name]} if path.suffix == ".json" else {}),
        }
        for path, content in sorted(documents.items(), key=lambda item: str(item[0]))
    }
    report = {
        "schema_version": CANDIDATE_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date_text,
        "participant_source_mode": participant_source_mode,
        "source_validation_sha256": _sha256(validation_report_path.read_bytes()),
        "publication": {"status": "disabled"},
        "outputs": output_metadata,
    }
    for path, content in documents.items():
        _atomic_write(output_root / path, content)
    _atomic_write(existing_report, _json_bytes(report))
    return report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--prediction-root", required=True, type=Path)
    parser.add_argument("--validation-report", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--output-root", required=True, type=Path)
    args = parser.parse_args()
    report = build_final_climb_candidate(
        prediction_root=args.prediction_root,
        validation_report_path=args.validation_report,
        logical_date=args.logical_date,
        output_root=args.output_root,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
