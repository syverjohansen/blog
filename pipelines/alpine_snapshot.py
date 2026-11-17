#!/usr/bin/env python3
"""Create and materialize immutable Alpine Parquet snapshot bundles."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import tempfile
from datetime import date, datetime, timezone
from pathlib import Path
from typing import Any

import polars as pl


SNAPSHOT_SCHEMA_VERSION = "nordicnumbers.alpine-input-snapshot/v1"
INPUTS = {
    "race_schedule": "races.csv",
    "men_rating_history": "men_chrono.csv",
    "ladies_rating_history": "ladies_chrono.csv",
    "men_startlist": "startlist_races_men.csv",
    "ladies_startlist": "startlist_races_ladies.csv",
}
BASE_INPUT_IDS = (
    "race_schedule",
    "men_rating_history",
    "ladies_rating_history",
)
STARTLIST_INPUT_IDS = ("men_startlist", "ladies_startlist")


class SnapshotError(RuntimeError):
    pass


def safe_filename(value: str) -> str:
    if not value or Path(value).name != value or value in {".", ".."}:
        raise SnapshotError(f"unsafe snapshot filename: {value!r}")
    return value


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def canonical_digest(value: Any) -> str:
    payload = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    return hashlib.sha256(payload).hexdigest()


def parse_logical_date(value: str) -> date:
    try:
        return date.fromisoformat(value)
    except ValueError as exc:
        raise SnapshotError("logical date must use YYYY-MM-DD") from exc


def read_source(path: Path) -> pl.DataFrame:
    try:
        return pl.read_csv(
            path,
            infer_schema_length=None,
            try_parse_dates=True,
            null_values=["", "NA", "N/A"],
        )
    except Exception as exc:
        raise SnapshotError(f"cannot parse {path}: {exc}") from exc


def create_snapshot(
    input_root: Path,
    output_root: Path,
    logical_date: date,
    *,
    allow_partial_startlists: bool = False,
) -> dict[str, Any]:
    required_ids = BASE_INPUT_IDS if allow_partial_startlists else tuple(INPUTS)
    missing = [
        INPUTS[dataset_id]
        for dataset_id in required_ids
        if not (input_root / INPUTS[dataset_id]).is_file()
    ]
    if missing:
        raise SnapshotError(f"missing Alpine inputs in {input_root}: {missing}")

    selected_inputs = {
        dataset_id: filename
        for dataset_id, filename in INPUTS.items()
        if (input_root / filename).is_file()
    }
    selected_startlists = set(selected_inputs) & set(STARTLIST_INPUT_IDS)
    if allow_partial_startlists and not selected_startlists:
        raise SnapshotError(f"no Alpine startlist exists in {input_root}")

    source_digests = {
        dataset_id: sha256_file(input_root / filename)
        for dataset_id, filename in selected_inputs.items()
    }
    identity = {
        "schema_version": SNAPSHOT_SCHEMA_VERSION,
        "logical_date": logical_date.isoformat(),
        "input_mode": (
            "applicable_startlists"
            if allow_partial_startlists
            else "complete_five_file_set"
        ),
        "source_sha256": source_digests,
    }
    snapshot_id = canonical_digest(identity)[:24]
    parent = output_root / "snapshots/alpine" / f"logical_date={logical_date.isoformat()}"
    snapshot_dir = parent / f"snapshot_id={snapshot_id}"
    manifest_path = snapshot_dir / "_manifest.json"

    if snapshot_dir.exists():
        if not manifest_path.is_file():
            raise SnapshotError(f"immutable snapshot exists without manifest: {snapshot_dir}")
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        if manifest.get("snapshot_id") != snapshot_id:
            raise SnapshotError(f"immutable snapshot identity mismatch: {snapshot_dir}")
        for dataset in manifest.get("datasets", []):
            parquet = snapshot_dir / safe_filename(dataset["parquet_file"])
            if not parquet.is_file() or sha256_file(parquet) != dataset["parquet_sha256"]:
                raise SnapshotError(f"immutable snapshot failed digest verification: {parquet}")
        manifest["manifest_sha256"] = sha256_file(manifest_path)
        manifest["status"] = "reused"
        manifest["manifest_path"] = str(manifest_path)
        return manifest

    parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=f".{snapshot_id}-", dir=parent) as staging_text:
        staging = Path(staging_text)
        datasets: list[dict[str, Any]] = []
        for dataset_id, filename in selected_inputs.items():
            frame = read_source(input_root / filename)
            parquet_name = f"{dataset_id}.parquet"
            parquet_path = staging / parquet_name
            frame.write_parquet(
                parquet_path,
                compression="zstd",
                statistics=True,
            )
            datasets.append(
                {
                    "dataset_id": dataset_id,
                    "source_file": filename,
                    "source_sha256": source_digests[dataset_id],
                    "parquet_file": parquet_name,
                    "parquet_sha256": sha256_file(parquet_path),
                    "rows": frame.height,
                    "columns": frame.width,
                    "schema": [
                        {"name": name, "type": str(dtype)}
                        for name, dtype in frame.schema.items()
                    ],
                }
            )
        manifest = {
            "schema_version": SNAPSHOT_SCHEMA_VERSION,
            "snapshot_id": snapshot_id,
            "logical_date": logical_date.isoformat(),
            "created_at": datetime.now(timezone.utc).isoformat(),
            "format": "parquet",
            "compression": "zstd",
            "immutability": "content-addressed",
            "input_mode": identity["input_mode"],
            "datasets": datasets,
        }
        (staging / "_manifest.json").write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        os.rename(staging, snapshot_dir)

    manifest["manifest_sha256"] = sha256_file(manifest_path)
    manifest["status"] = "created"
    manifest["manifest_path"] = str(manifest_path)
    return manifest


def materialize_snapshot(snapshot_dir: Path, output_root: Path) -> dict[str, Any]:
    manifest_path = snapshot_dir / "_manifest.json"
    if not manifest_path.is_file():
        raise SnapshotError(f"snapshot manifest is missing: {manifest_path}")
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("schema_version") != SNAPSHOT_SCHEMA_VERSION:
        raise SnapshotError(f"unsupported snapshot schema: {manifest.get('schema_version')}")

    output_root.mkdir(parents=True, exist_ok=True)
    outputs: list[dict[str, Any]] = []
    for dataset in manifest["datasets"]:
        parquet = snapshot_dir / safe_filename(dataset["parquet_file"])
        if sha256_file(parquet) != dataset["parquet_sha256"]:
            raise SnapshotError(f"Parquet digest mismatch: {parquet}")
        output = output_root / dataset["source_file"]
        frame = pl.read_parquet(parquet)
        if frame.height != dataset["rows"] or frame.width != dataset["columns"]:
            raise SnapshotError(f"Parquet shape mismatch: {parquet}")
        observed_schema = [
            {"name": name, "type": str(dtype)} for name, dtype in frame.schema.items()
        ]
        if observed_schema != dataset.get("schema"):
            raise SnapshotError(f"Parquet schema mismatch: {parquet}")
        frame.write_csv(output)
        outputs.append(
            {
                "dataset_id": dataset["dataset_id"],
                "file": dataset["source_file"],
                "rows": frame.height,
                "sha256": sha256_file(output),
            }
        )
    return {
        "schema_version": SNAPSHOT_SCHEMA_VERSION,
        "snapshot_id": manifest["snapshot_id"],
        "status": "materialized",
        "outputs": outputs,
    }


def write_result(result: dict[str, Any], path: Path | None) -> None:
    payload = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if path:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(payload, encoding="utf-8")
    else:
        print(payload, end="")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    create = subparsers.add_parser("create")
    create.add_argument("--input-root", required=True, type=Path)
    create.add_argument("--output-root", required=True, type=Path)
    create.add_argument("--logical-date", required=True)
    create.add_argument("--allow-partial-startlists", action="store_true")
    create.add_argument("--report", type=Path)
    materialize = subparsers.add_parser("materialize")
    materialize.add_argument("--snapshot-dir", required=True, type=Path)
    materialize.add_argument("--output-root", required=True, type=Path)
    materialize.add_argument("--report", type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        if args.command == "create":
            result = create_snapshot(
                args.input_root.expanduser().resolve(),
                args.output_root.expanduser().resolve(),
                parse_logical_date(args.logical_date),
                allow_partial_startlists=args.allow_partial_startlists,
            )
        else:
            result = materialize_snapshot(
                args.snapshot_dir.expanduser().resolve(),
                args.output_root.expanduser().resolve(),
            )
        write_result({"status": "passed", "snapshot": result}, args.report)
        return 0
    except (SnapshotError, OSError, json.JSONDecodeError) as exc:
        write_result({"status": "failed", "error": str(exc)}, args.report)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
