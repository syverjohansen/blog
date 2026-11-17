#!/usr/bin/env python3
"""Verify the immutable input set used by historical Alpine parity tests."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from datetime import date
from pathlib import Path
from typing import Any


SCHEMA_VERSION = "nordicnumbers.alpine-parity-inputs/v1"
REQUIRED_FILES = {
    "races.csv",
    "men_chrono.csv",
    "ladies_chrono.csv",
    "startlist_races_men.csv",
    "startlist_races_ladies.csv",
}


class PinnedInputError(RuntimeError):
    """The historical parity input set is incomplete or has changed."""


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def load_manifest(path: Path) -> dict[str, Any]:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise PinnedInputError(
            f"cannot read pinned-input manifest {path}: {exc}"
        ) from exc
    if document.get("schema_version") != SCHEMA_VERSION:
        raise PinnedInputError(
            f"unsupported pinned-input schema: {document.get('schema_version')!r}"
        )
    commit = document.get("ski_commit_sha", "")
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise PinnedInputError("ski_commit_sha must be a full lowercase Git SHA")
    try:
        date.fromisoformat(document["logical_date"])
    except (KeyError, TypeError, ValueError) as exc:
        raise PinnedInputError("logical_date must use YYYY-MM-DD") from exc
    files = document.get("files")
    if not isinstance(files, dict) or set(files) != REQUIRED_FILES:
        raise PinnedInputError(
            f"pinned files must be exactly {sorted(REQUIRED_FILES)}"
        )
    invalid_hashes = [
        name
        for name, digest in files.items()
        if not isinstance(digest, str) or not re.fullmatch(r"[0-9a-f]{64}", digest)
    ]
    if invalid_hashes:
        raise PinnedInputError(
            f"invalid SHA-256 values for: {sorted(invalid_hashes)}"
        )
    return document


def verify_inputs(manifest_path: Path, input_root: Path) -> dict[str, Any]:
    manifest = load_manifest(manifest_path)
    observed_files = {
        path.name
        for path in input_root.iterdir()
        if path.is_file()
    } if input_root.is_dir() else set()
    if observed_files != REQUIRED_FILES:
        raise PinnedInputError(
            "input directory files differ from manifest: "
            f"expected {sorted(REQUIRED_FILES)}, observed {sorted(observed_files)}"
        )
    verified = []
    for name, expected in manifest["files"].items():
        path = input_root / name
        observed = sha256_file(path)
        if observed != expected:
            raise PinnedInputError(
                f"SHA-256 mismatch for {name}: expected {expected}, observed {observed}"
            )
        verified.append({"file": name, "sha256": observed})
    return {
        "status": "passed",
        "logical_date": manifest["logical_date"],
        "ski_commit_sha": manifest["ski_commit_sha"],
        "files": verified,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--input-root", required=True, type=Path)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    try:
        result = verify_inputs(
            args.manifest.expanduser().resolve(),
            args.input_root.expanduser().resolve(),
        )
    except PinnedInputError as exc:
        print(json.dumps({"status": "failed", "error": str(exc)}, indent=2))
        return 1
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
