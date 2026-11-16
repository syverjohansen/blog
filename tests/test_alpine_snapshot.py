from __future__ import annotations

import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.alpine_snapshot import (
    INPUTS,
    SnapshotError,
    create_snapshot,
    materialize_snapshot,
)


class AlpineSnapshotTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="alpine-snapshot-unit-")
        self.root = Path(self.temporary.name)
        self.inputs = self.root / "inputs"
        self.store = self.root / "store"
        self.inputs.mkdir()
        for filename in INPUTS.values():
            (self.inputs / filename).write_text(
                "ID,Date,Value\n1,2026-03-25,10.5\n2,2026-03-25,20.5\n",
                encoding="utf-8",
            )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def test_create_reuse_and_materialize(self) -> None:
        first = create_snapshot(self.inputs, self.store, date(2026, 3, 25))
        second = create_snapshot(self.inputs, self.store, date(2026, 3, 25))
        self.assertEqual("created", first["status"])
        self.assertEqual("reused", second["status"])
        self.assertEqual(first["snapshot_id"], second["snapshot_id"])
        self.assertEqual(first["manifest_sha256"], second["manifest_sha256"])

        materialized = self.root / "materialized"
        result = materialize_snapshot(
            Path(first["manifest_path"]).parent,
            materialized,
        )
        self.assertEqual("materialized", result["status"])
        self.assertEqual(
            set(INPUTS.values()), {item["file"] for item in result["outputs"]}
        )

    def test_tampered_object_is_rejected(self) -> None:
        snapshot = create_snapshot(self.inputs, self.store, date(2026, 3, 25))
        parquet = Path(snapshot["manifest_path"]).parent / "race_schedule.parquet"
        with parquet.open("ab") as handle:
            handle.write(b"tampered")
        with self.assertRaisesRegex(SnapshotError, "digest verification"):
            create_snapshot(self.inputs, self.store, date(2026, 3, 25))

    def test_materialize_rejects_unsafe_manifest_filename(self) -> None:
        snapshot = create_snapshot(self.inputs, self.store, date(2026, 3, 25))
        manifest_path = Path(snapshot["manifest_path"])
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        manifest["datasets"][0]["parquet_file"] = "../escape.parquet"
        manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
        with self.assertRaisesRegex(SnapshotError, "unsafe snapshot filename"):
            materialize_snapshot(manifest_path.parent, self.root / "materialized")

    def test_materialize_rejects_manifest_schema_drift(self) -> None:
        snapshot = create_snapshot(self.inputs, self.store, date(2026, 3, 25))
        manifest_path = Path(snapshot["manifest_path"])
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        manifest["datasets"][0]["schema"][0]["type"] = "String"
        manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
        with self.assertRaisesRegex(SnapshotError, "schema mismatch"):
            materialize_snapshot(manifest_path.parent, self.root / "materialized")


if __name__ == "__main__":
    unittest.main()
