"""Build guarded Nordic Combined prediction JSON and a Hugo post from a passed shadow."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import tempfile
from datetime import UTC, date, datetime
from pathlib import Path
from typing import Any

from openpyxl import load_workbook

from pipelines.nordic_combined_prediction_contract import CATEGORY_CONTRACTS
from pipelines.nordic_combined_prediction_shadow import (
    INDIVIDUAL_COLUMNS,
    NORMALIZATION_SCHEMA,
    TEAM_COLUMNS,
)
from pipelines.nordic_combined_prediction_shadow import (
    SCHEMA_VERSION as SHADOW_SCHEMA,
)

PUBLICATION_SCHEMA = "nordicnumbers.nordic-combined-prediction-publication/v1"
CATEGORY_LABELS = {
    "men": "Men",
    "ladies": "Ladies",
    "men_team": "Men Team",
    "ladies_team": "Ladies Team",
    "men_team_sprint": "Men Team Sprint",
    "ladies_team_sprint": "Ladies Team Sprint",
    "mixed_team": "Mixed Team",
    "mixed_team_sprint": "Mixed Team Sprint",
}
PROBABILITY_COLUMNS = ("Win", "Podium", "Top-5", "Top-10", "Top-30")


class NordicCombinedPublicationError(RuntimeError):
    """Raised when shadow evidence is not safe to publish."""


def _load_json(path: Path, label: str) -> dict[str, Any]:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise NordicCombinedPublicationError(f"cannot read {label}: {path}") from error
    if not isinstance(document, dict):
        raise NordicCombinedPublicationError(f"{label} must be a JSON object")
    return document


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _json_bytes(document: dict[str, Any]) -> bytes:
    return (json.dumps(document, indent=2, ensure_ascii=False) + "\n").encode()


def _write_atomic(path: Path, payload: bytes) -> None:
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


def _native(value: Any) -> Any:
    if value is None:
        return ""
    if isinstance(value, (date, datetime)):
        return value.isoformat()
    return value


def _workbook_documents(path: Path) -> list[dict[str, Any]]:
    workbook = load_workbook(path, read_only=True, data_only=True)
    documents: list[dict[str, Any]] = []
    try:
        for sheet_name in workbook.sheetnames:
            sheet = workbook[sheet_name]
            if hasattr(sheet, "reset_dimensions"):
                sheet.reset_dimensions()
            source_rows = sheet.iter_rows(values_only=True)
            header = tuple(next(source_rows, ()))
            rows = [
                [_native(value) for value in row]
                for row in source_rows
                if not all(value is None for value in row)
            ]
            documents.append(
                {"sheet_name": sheet_name, "headers": list(header), "rows": rows}
            )
    finally:
        workbook.close()
    return documents


def _validate_document(document: dict[str, Any], category: str) -> None:
    expected = TEAM_COLUMNS if category not in {"men", "ladies"} else INDIVIDUAL_COLUMNS
    headers = document["headers"]
    rows = document["rows"]
    if tuple(headers) != expected or not rows:
        raise NordicCombinedPublicationError(
            f"invalid converted schema or empty rows for {category}/{document['sheet_name']}"
        )
    indexes = [headers.index(column) for column in PROBABILITY_COLUMNS]
    win_total = 0.0
    for row in rows:
        if len(row) != len(headers):
            raise NordicCombinedPublicationError(
                f"converted row width mismatch for {category}/{document['sheet_name']}"
            )
        try:
            probabilities = [float(row[index]) for index in indexes]
        except (TypeError, ValueError) as error:
            raise NordicCombinedPublicationError(
                f"nonnumeric probability for {category}/{document['sheet_name']}"
            ) from error
        if probabilities != sorted(probabilities) or any(
            value < 0 or value > 100 for value in probabilities
        ):
            raise NordicCombinedPublicationError(
                f"invalid probability semantics for {category}/{document['sheet_name']}"
            )
        win_total += probabilities[0]
    if not 95 <= win_total <= 105:
        raise NordicCombinedPublicationError(
            f"winner probabilities do not sum to 100 for {category}/{document['sheet_name']}"
        )


def _output_name(workbook: Path, sheet_name: str, sheet_count: int) -> str:
    if sheet_count == 1:
        return f"{workbook.stem}.json"
    safe_sheet = "".join(
        character if character.isalnum() else "_" for character in sheet_name
    )
    return f"{workbook.stem}_{safe_sheet}.json"


def _render_post(logical_date: date, artifacts: list[dict[str, Any]]) -> bytes:
    date_key = logical_date.strftime("%Y%m%d")
    lines = [
        "---",
        f'title: "Nordic Combined Race Picks for {logical_date.strftime("%m/%d/%Y")}"',
        f"date: {logical_date.isoformat()}T06:00:00+00:00",
        "draft: false",
        'tags: ["predictions", "skiing", "race-picks", "nordic-combined"]',
        "---",
        "",
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details on how these predictions are generated.*",
        "",
        "## Nordic Combined",
        "",
    ]
    grouped = {
        category: [item for item in artifacts if item["category"] == category]
        for category in CATEGORY_CONTRACTS
    }
    for category, items in grouped.items():
        if not items:
            continue
        lines.extend([f"### {CATEGORY_LABELS[category]}", ""])
        for item in items:
            if len(items) > 1:
                lines.extend([f"#### {item['sheet_name']}", ""])
            lines.extend(
                [
                    "#### Position Probabilities"
                    if len(items) == 1
                    else "##### Position Probabilities",
                    "",
                    f'{{{{< nordic-combined/datatable "nordic-combined/drafts/race-picks/{date_key}/{Path(item["file"]).stem}" >}}}}',
                    "",
                ]
            )
    return ("\n".join(lines).rstrip() + "\n").encode()


def prepare_publication(
    *,
    shadow_report_path: Path,
    normalization_report_path: Path,
    workbook_root: Path,
    repository_root: Path,
    report_path: Path,
    evaluation_date: date | None = None,
) -> dict[str, Any]:
    shadow = _load_json(shadow_report_path, "shadow report")
    if (
        shadow.get("schema_version") != SHADOW_SCHEMA
        or shadow.get("status") != "passed"
    ):
        raise NordicCombinedPublicationError("only a passed Nordic Combined shadow can publish")
    if shadow.get("publication", {}).get("status") != "disabled":
        raise NordicCombinedPublicationError("shadow publication lock was not retained")
    if shadow.get("startlist_source") != "normalized_official_fis":
        raise NordicCombinedPublicationError(
            "shadow did not use normalized official FIS startlists"
        )
    logical_date = date.fromisoformat(str(shadow["logical_date"]))
    evaluation_date = evaluation_date or datetime.now(UTC).date()
    if logical_date < evaluation_date:
        raise NordicCombinedPublicationError("past prediction runs cannot be published")

    normalization = _load_json(normalization_report_path, "normalization report")
    declared_normalization = shadow.get("startlist_normalization_report", {})
    if (
        normalization.get("schema_version") != NORMALIZATION_SCHEMA
        or normalization.get("logical_date") != logical_date.isoformat()
        or normalization.get("status") != "ready"
        or normalization.get("prediction_eligible") is not True
        or normalization.get("accepted_startlists_modified") is not False
        or declared_normalization.get("sha256") != _sha256(normalization_report_path)
    ):
        raise NordicCombinedPublicationError(
            "normalization evidence does not match the shadow"
        )

    run_root = shadow_report_path.resolve().parents[1]
    plan = _load_json(run_root / str(shadow.get("plan", "")), "execution plan")
    if (
        plan.get("status") != "ready_for_shadow_implementation"
        or plan.get("logical_date") != logical_date.isoformat()
    ):
        raise NordicCombinedPublicationError("execution plan is not publication eligible")
    active_categories = {
        category
        for category, count in plan.get("categories", {}).items()
        if int(count) > 0
    }
    expected_workbooks = {
        str(CATEGORY_CONTRACTS[category]["workbook"]): category
        for category in active_categories
    }
    declared_outputs = {
        str(item.get("file")): item
        for item in shadow.get("outputs", {}).get("workbooks", [])
    }
    if set(declared_outputs) != set(expected_workbooks):
        raise NordicCombinedPublicationError("shadow workbook set does not match the plan")

    date_key = logical_date.strftime("%Y%m%d")
    target_data_root = repository_root / "data/nordic-combined/drafts/race-picks" / date_key
    planned_files: list[tuple[Path, bytes]] = []
    artifacts: list[dict[str, Any]] = []
    for filename, category in sorted(expected_workbooks.items()):
        workbook = workbook_root / filename
        declared = declared_outputs[filename]
        if not workbook.is_file() or declared.get("sha256") != _sha256(workbook):
            raise NordicCombinedPublicationError(f"workbook hash mismatch: {filename}")
        documents = _workbook_documents(workbook)
        declared_sheets = declared.get("sheets")
        observed_sheets = {
            document["sheet_name"]: len(document["rows"]) for document in documents
        }
        if declared_sheets != observed_sheets:
            raise NordicCombinedPublicationError(f"workbook sheet parity failed: {filename}")
        for document in documents:
            _validate_document(document, category)
            output_name = _output_name(workbook, document["sheet_name"], len(documents))
            payload = _json_bytes(document)
            target = target_data_root / output_name
            planned_files.append((target, payload))
            artifacts.append(
                {
                    "category": category,
                    "sheet_name": document["sheet_name"],
                    "file": str(target.relative_to(repository_root)),
                    "rows": len(document["rows"]),
                    "sha256": hashlib.sha256(payload).hexdigest(),
                    "parity": "passed",
                }
            )

    post_path = (
        repository_root
        / "race-picks/fragments"
        / date_key
        / "nordic-combined.md"
    )
    planned_files.append((post_path, _render_post(logical_date, artifacts)))
    for target, payload in planned_files:
        if target.exists() and target.read_bytes() != payload:
            raise NordicCombinedPublicationError(
                f"publication target already exists with different content: {target}"
            )
    states = []
    for target, payload in planned_files:
        state = "unchanged" if target.exists() else "created"
        if state == "created":
            _write_atomic(target, payload)
        states.append(
            {"file": str(target.relative_to(repository_root)), "status": state}
        )

    report = {
        "schema_version": PUBLICATION_SCHEMA,
        "status": "prepared"
        if any(item["status"] == "created" for item in states)
        else "unchanged",
        "logical_date": logical_date.isoformat(),
        "source_run_id": shadow.get("run_id"),
        "source_commits": shadow.get("code"),
        "quality": {
            "workbook_hashes": "passed",
            "sheet_parity": "passed",
            "json_semantics": "passed",
            "official_startlists": "passed",
        },
        "artifacts": artifacts,
        "files": states,
        "publication": {"status": "ready"},
    }
    _write_atomic(report_path, _json_bytes(report))
    return report


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--shadow-report", required=True, type=Path)
    parser.add_argument("--normalization-report", required=True, type=Path)
    parser.add_argument("--workbook-root", required=True, type=Path)
    parser.add_argument("--repository-root", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        report = prepare_publication(
            shadow_report_path=args.shadow_report,
            normalization_report_path=args.normalization_report,
            workbook_root=args.workbook_root,
            repository_root=args.repository_root,
            report_path=args.report,
        )
    except (KeyError, OSError, ValueError, NordicCombinedPublicationError) as error:
        print(str(error), file=os.sys.stderr)
        return 1
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

