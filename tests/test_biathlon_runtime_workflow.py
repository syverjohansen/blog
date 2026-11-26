from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/biathlon-runtime-shadow.yml"


class BiathlonRuntimeWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")
        cls.trigger = cls.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]

    def test_workflow_is_manual_only_and_read_only(self) -> None:
        self.assertIn("workflow_dispatch:", self.trigger)
        self.assertNotIn("schedule:", self.trigger)
        self.assertNotIn("workflow_run:", self.trigger)
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertNotIn("contents: write", self.source)

    def test_ski_source_is_resolved_and_checked_out_exactly(self) -> None:
        self.assertIn('gh api "repos/syverjohansen/ski/commits/$SKI_REF"', self.source)
        self.assertIn('[[ "$ski_sha" =~ ^[0-9a-f]{40}$ ]]', self.source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", self.source)
        self.assertIn("persist-credentials: false", self.source)
        self.assertIn("/elo/python/biathlon/polars", self.source)

    def test_runtime_uses_pinned_dependencies_and_safe_race_arguments(self) -> None:
        self.assertIn("requirements-elo-parity.txt", self.source)
        self.assertIn("certifi==2026.11.20", self.source)
        self.assertIn('split(",")', self.source)
        self.assertIn('race_args+=(--race-id "$race_id")', self.source)
        self.assertNotIn("eval ", self.source)
        self.assertIn("--code-commit \"$SKI_SHA\"", self.source)

    def test_failure_evidence_is_always_uploaded_before_enforcement(self) -> None:
        paths = self.source.index("Prepare immutable run paths")
        resolve = self.source.index("Resolve ski source to an exact commit")
        runner = self.source.index("id: run_shadow")
        receipt = self.source.index("Retain wrapper failure evidence")
        upload = self.source.index("Upload complete Biathlon shadow evidence")
        enforcement = self.source.index("Enforce retained shadow result")
        self.assertLess(paths, resolve)
        self.assertLess(runner, receipt)
        self.assertLess(receipt, upload)
        self.assertLess(upload, enforcement)
        self.assertIn("continue-on-error: true", self.source[runner:receipt])
        self.assertIn("if: always()", self.source[receipt:enforcement])
        self.assertIn("retention-days: 30", self.source)
        self.assertIn("SKI_SHA_VALUE: ${{ steps.ski.outputs.sha || 'unresolved' }}", self.source)

    def test_workflow_enforces_all_runtime_safety_contracts(self) -> None:
        self.assertIn('report.get("status") != "passed"', self.source)
        self.assertIn('report.get("accepted_data_mutated") is not False', self.source)
        self.assertIn('report.get("promotion") != {"status": "disabled"}', self.source)
        self.assertIn('report.get("publication") != {"status": "disabled"}', self.source)
        self.assertIn('report.get("code_commit") != os.environ["SKI_SHA"]', self.source)
        self.assertIn('"rebuild_relay_team_artifacts"', self.source)

    def test_workflow_contains_no_promotion_or_publication_write(self) -> None:
        self.assertNotIn("git push", self.source)
        self.assertNotIn("create-pull-request", self.source)
        self.assertNotIn("gh pr create", self.source)
        self.assertNotIn("actions/checkout@v4\n        with:\n          token:", self.source)


if __name__ == "__main__":
    unittest.main()
