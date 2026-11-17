from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from pipelines.alpine_smoke_evidence import write_failure_evidence


class AlpineSmokeEvidenceTests(unittest.TestCase):
    def test_failure_writes_atomic_publication_disabled_manifest(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-smoke-evidence-") as text:
            root = Path(text)
            output = root / "failure.json"

            written = write_failure_evidence(
                output_path=output,
                successful_manifest=root / "success.json",
                logical_date="2026-11-23",
                blog_sha="blog-sha",
                ski_sha="ski-sha",
                task_statuses={
                    "build_isolated_current_inputs": "failure",
                    "run_current_season_smoke": "skipped",
                },
                log_path="artifacts/alpine-input-build.log",
            )

            payload = json.loads(output.read_text(encoding="utf-8"))
            self.assertTrue(written)
            self.assertEqual("failed", payload["status"])
            self.assertEqual("disabled", payload["publication"]["status"])
            self.assertEqual("ski-sha", payload["code"]["ski"]["commit_sha"])
            self.assertEqual(
                [
                    {
                        "status": "failure",
                        "task_id": "build_isolated_current_inputs",
                    },
                    {
                        "status": "skipped",
                        "task_id": "run_current_season_smoke",
                    },
                ],
                payload["task_outcomes"],
            )

    def test_success_manifest_suppresses_failure_evidence(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-smoke-success-") as text:
            root = Path(text)
            success = root / "success.json"
            success.write_text("{}\n", encoding="utf-8")
            output = root / "failure.json"

            written = write_failure_evidence(
                output_path=output,
                successful_manifest=success,
                logical_date="2026-11-14",
                blog_sha="blog-sha",
                ski_sha="ski-sha",
                task_statuses={"run_current_season_smoke": "success"},
                log_path="artifacts/alpine-input-build.log",
            )

            self.assertFalse(written)
            self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
