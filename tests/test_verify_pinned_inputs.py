from __future__ import annotations

import hashlib
import json
import tempfile
import unittest
from pathlib import Path

from pipelines.verify_pinned_inputs import (
    REQUIRED_FILES,
    SCHEMA_VERSION,
    PinnedInputError,
    load_manifest,
    verify_inputs,
)


ROOT = Path(__file__).resolve().parents[1]
REPOSITORY_MANIFEST = ROOT / "tests/pipeline/alpine-parity-inputs.json"


class VerifyPinnedInputsTests(unittest.TestCase):
    def build_fixture(self, root: Path) -> tuple[Path, Path]:
        input_root = root / "inputs"
        input_root.mkdir()
        hashes = {}
        for name in REQUIRED_FILES:
            payload = f"fixture for {name}\n".encode()
            (input_root / name).write_bytes(payload)
            hashes[name] = hashlib.sha256(payload).hexdigest()
        manifest = root / "manifest.json"
        manifest.write_text(
            json.dumps(
                {
                    "schema_version": SCHEMA_VERSION,
                    "logical_date": "2026-03-25",
                    "ski_commit_sha": "a" * 40,
                    "files": hashes,
                }
            ),
            encoding="utf-8",
        )
        return manifest, input_root

    def test_repository_manifest_has_the_complete_contract(self) -> None:
        document = load_manifest(REPOSITORY_MANIFEST)
        self.assertEqual(REQUIRED_FILES, set(document["files"]))
        self.assertEqual("2026-03-25", document["logical_date"])

    def test_exact_input_set_passes(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-pinned-inputs-") as text:
            manifest, input_root = self.build_fixture(Path(text))
            result = verify_inputs(manifest, input_root)
            self.assertEqual("passed", result["status"])
            self.assertEqual(len(REQUIRED_FILES), len(result["files"]))

    def test_changed_file_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-pinned-drift-") as text:
            manifest, input_root = self.build_fixture(Path(text))
            (input_root / "races.csv").write_text("changed\n", encoding="utf-8")
            with self.assertRaisesRegex(PinnedInputError, "SHA-256 mismatch"):
                verify_inputs(manifest, input_root)

    def test_extra_file_fails_closed(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-pinned-extra-") as text:
            manifest, input_root = self.build_fixture(Path(text))
            (input_root / "unexpected.csv").write_text("extra\n", encoding="utf-8")
            with self.assertRaisesRegex(PinnedInputError, "differ from manifest"):
                verify_inputs(manifest, input_root)


if __name__ == "__main__":
    unittest.main()
