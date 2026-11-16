from __future__ import annotations

import unittest
from pathlib import Path


BLOG_ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = BLOG_ROOT / ".github/workflows/alpine-shadow.yml"


class AlpineShadowWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.source = WORKFLOW.read_text(encoding="utf-8")

    def test_workflow_is_manual_only(self) -> None:
        trigger = self.source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertNotIn("push:", trigger)
        self.assertNotIn("pull_request:", trigger)

    def test_workflow_has_read_only_repository_permissions(self) -> None:
        self.assertIn("permissions:\n  contents: read", self.source)
        self.assertNotIn("contents: write", self.source)

    def test_workflow_runs_real_gate_and_enforces_disabled_publication(self) -> None:
        self.assertIn("test_pipeline.py alpine-integration", self.source)
        self.assertIn("alpine-shadow-manifest.json", self.source)
        self.assertIn('publication.get("status") != "disabled"', self.source)
        self.assertNotIn("git push", self.source)
        self.assertNotIn("aws s3", self.source)

    def test_ski_checkout_is_bounded_to_alpine_inputs(self) -> None:
        self.assertIn("raw.githubusercontent.com/syverjohansen/ski", self.source)
        self.assertIn("races.csv men_chrono.csv ladies_chrono.csv", self.source)
        self.assertIn("--ski-input-root", self.source)
        self.assertIn("--ski-commit-sha", self.source)

    def test_blog_checkout_is_bounded_to_runtime_files(self) -> None:
        self.assertIn("sparse-checkout-cone-mode: false", self.source)
        self.assertIn("/pipelines/", self.source)
        self.assertIn("/content/post/shared/", self.source)
        self.assertIn("/themes/white/", self.source)


if __name__ == "__main__":
    unittest.main()
