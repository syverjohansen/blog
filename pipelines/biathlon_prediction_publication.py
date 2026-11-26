"""Publish parity-checked Biathlon prediction JSON and a sport-specific Hugo post."""

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

from pipelines.biathlon_prediction_shadow import (
    CATEGORIES,
    NORMALIZATION_SCHEMA,
    SCHEMA_VERSION as SHADOW_SCHEMA,
)


SCHEMA_VERSION = "nordicnumbers.biathlon-prediction-publication/v1"
LABELS = {
    "men": "Men", "ladies": "Ladies", "men_relay": "Men's Relay",
    "ladies_relay": "Ladies' Relay", "mixed_relay": "Mixed Relay",
    "single_mixed_relay": "Single Mixed Relay",
}


class BiathlonPublicationError(RuntimeError):
    """Passed shadow evidence is incomplete or unsafe to publish."""


def _load(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise BiathlonPublicationError(f"cannot read JSON evidence: {path}") from error
    if not isinstance(value, dict):
        raise BiathlonPublicationError(f"JSON evidence must be an object: {path}")
    return value


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _atomic(path: Path, payload: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    temporary = Path(name)
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


def _documents(path: Path) -> list[dict[str, Any]]:
    workbook = load_workbook(path, read_only=True, data_only=True)
    documents: list[dict[str, Any]] = []
    try:
        for name in workbook.sheetnames:
            sheet = workbook[name]
            if hasattr(sheet, "reset_dimensions"):
                sheet.reset_dimensions()
            rows = sheet.iter_rows(values_only=True)
            headers = [_native(value) for value in next(rows, ())]
            body = [
                [_native(value) for value in row]
                for row in rows if any(value is not None for value in row)
            ]
            documents.append({"sheet_name": name, "headers": headers, "rows": body})
    finally:
        workbook.close()
    return documents


def _output_name(workbook: str, sheet: str, count: int) -> str:
    stem = Path(workbook).stem
    if count == 1:
        return f"{stem}.json"
    safe = "".join(character if character.isalnum() else "_" for character in sheet)
    return f"{stem}_{safe}.json"


def _post(logical_date: date, artifacts: list[dict[str, Any]]) -> bytes:
    date_key = logical_date.strftime("%Y%m%d")
    lines = [
        "---",
        f'title: "Biathlon Race Picks for {logical_date.strftime("%m/%d/%Y")}"',
        f"date: {logical_date.isoformat()}T06:00:00+00:00",
        "draft: false",
        'tags: ["predictions", "skiing", "race-picks", "biathlon"]',
        "---", "",
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details on how these predictions are generated.*",
        "", "## Biathlon", "",
    ]
    for category in CATEGORIES:
        items = [item for item in artifacts if item["category"] == category]
        if not items:
            continue
        lines.extend([f"### {LABELS[category]}", ""])
        for item in items:
            if len(items) > 1:
                lines.extend([f"#### {item['sheet_name']}", ""])
            lines.extend([
                "#### Position Probabilities" if len(items) == 1 else "##### Position Probabilities",
                "",
                f'{{{{< biathlon/datatable "biathlon/drafts/race-picks/{date_key}/{Path(item["file"]).stem}" >}}}}',
                "",
            ])
    return ("\n".join(lines).rstrip() + "\n").encode()


def prepare_publication(
    *, shadow_report: Path, normalization_report: Path, workbook_root: Path,
    repository_root: Path, report_path: Path, evaluation_date: date | None = None,
) -> dict[str, Any]:
    shadow = _load(shadow_report)
    if (
        shadow.get("schema_version") != SHADOW_SCHEMA
        or shadow.get("status") != "passed"
        or shadow.get("publication", {}).get("status") != "disabled"
        or shadow.get("startlist_source") != "normalized_official_ibu"
    ):
        raise BiathlonPublicationError("only a passed official-IBU shadow can publish")
    logical_date = date.fromisoformat(shadow["logical_date"])
    if logical_date < (evaluation_date or datetime.now(UTC).date()):
        raise BiathlonPublicationError("past prediction runs cannot be published")
    normalization = _load(normalization_report)
    declared = shadow.get("startlist_normalization_report", {})
    if (
        normalization.get("schema_version") != NORMALIZATION_SCHEMA
        or normalization.get("status") != "ready"
        or normalization.get("prediction_eligible") is not True
        or normalization.get("logical_date") != logical_date.isoformat()
        or normalization.get("accepted_startlists_modified") is not False
        or declared.get("sha256") != _sha256(normalization_report)
    ):
        raise BiathlonPublicationError("normalization evidence does not match the shadow")
    run_root = shadow_report.resolve().parents[1]
    plan = _load(run_root / shadow["plan"])
    active = {category for category, count in plan["categories"].items() if count}
    expected = {CATEGORIES[category][1]: category for category in active}
    outputs = {item["file"]: item for item in shadow.get("outputs", [])}
    if set(outputs) != set(expected):
        raise BiathlonPublicationError("shadow output set does not match its plan")

    date_key = logical_date.strftime("%Y%m%d")
    data_root = repository_root / "data/biathlon/drafts/race-picks" / date_key
    writes: list[tuple[Path, bytes]] = []
    artifacts: list[dict[str, Any]] = []
    for filename, category in sorted(expected.items()):
        workbook = workbook_root / filename
        if not workbook.is_file() or outputs[filename]["sha256"] != _sha256(workbook):
            raise BiathlonPublicationError(f"workbook hash mismatch: {filename}")
        documents = _documents(workbook)
        observed = {item["sheet_name"]: len(item["rows"]) for item in documents}
        if outputs[filename]["sheets"] != observed:
            raise BiathlonPublicationError(f"workbook sheet parity failed: {filename}")
        for document in documents:
            name = _output_name(filename, document["sheet_name"], len(documents))
            target = data_root / name
            payload = (json.dumps(document, ensure_ascii=False, indent=2) + "\n").encode()
            writes.append((target, payload))
            artifacts.append({
                "category": category, "sheet_name": document["sheet_name"],
                "file": str(target.relative_to(repository_root)),
                "rows": len(document["rows"]), "sha256": hashlib.sha256(payload).hexdigest(),
            })
    post = repository_root / "content/post/race-picks" / f"biathlon-{date_key}.md"
    writes.append((post, _post(logical_date, artifacts)))
    expected_data = {target.resolve() for target, _ in writes if target.parent == data_root}
    if data_root.exists():
        for stale in data_root.glob("*.json"):
            if stale.resolve() not in expected_data:
                stale.unlink()
    for target, payload in writes:
        _atomic(target, payload)
    result = {
        "schema_version": SCHEMA_VERSION, "status": "prepared",
        "logical_date": logical_date.isoformat(), "artifacts": artifacts,
        "post": str(post.relative_to(repository_root)),
        "shadow_report_sha256": _sha256(shadow_report),
        "normalization_report_sha256": _sha256(normalization_report),
    }
    _atomic(report_path, (json.dumps(result, indent=2, sort_keys=True) + "\n").encode())
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--shadow-report", type=Path, required=True)
    parser.add_argument("--normalization-report", type=Path, required=True)
    parser.add_argument("--workbook-root", type=Path, required=True)
    parser.add_argument("--repository-root", type=Path, required=True)
    parser.add_argument("--report", type=Path, required=True)
    args = parser.parse_args()
    prepare_publication(
        shadow_report=args.shadow_report, normalization_report=args.normalization_report,
        workbook_root=args.workbook_root, repository_root=args.repository_root,
        report_path=args.report,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
