from __future__ import annotations

import csv
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-results-update.yml"


class NordicCombinedResultsWorkflowTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.text = WORKFLOW.read_text(encoding="utf-8")

    def test_follows_schedule_and_has_manual_recovery(self) -> None:
        self.assertIn(
            'workflows: ["Nordic-combined automatic schedule pipeline"]', self.text
        )
        self.assertIn("workflow_dispatch:", self.text)
        self.assertIn("workflow_run.conclusion == 'success'", self.text)

    def test_checkout_is_bounded_to_nordic_combined(self) -> None:
        self.assertIn("/elo/python/nordic-combined/polars/", self.text)
        self.assertNotIn("/elo/python/alpine/", self.text)
        self.assertNotIn("/elo/python/skijump/", self.text)

    def test_every_guarded_stage_runs_in_order(self) -> None:
        commands = [
            "nordic_combined_results_gap.py",
            "nordic_combined_results_acquire.py",
            "nordic_combined_results_identity.py",
            "nordic_combined_results_replay.py",
            "nordic_combined_results_promote.py",
            "nordic_combined_results_apply.py",
        ]
        positions = [self.text.index(command) for command in commands]
        self.assertEqual(positions, sorted(positions))
        self.assertIn("--fail-on-unresolved", self.text)
        self.assertGreaterEqual(self.text.count("--expected-report-sha256"), 2)

    def test_regular_individual_and_mixed_team_sprint_are_supported(self) -> None:
        self.assertIn("Enforce supported replay boundary", self.text)
        self.assertIn('event[1] in {"M", "L"}', self.text)
        self.assertIn('event[4] == "individual"', self.text)
        self.assertIn('event[1] == "Mixed"', self.text)
        self.assertIn('event[4] == "team_sprint"', self.text)
        self.assertIn('event[6] == "0"', self.text)

    def _run_boundary(
        self, *, sex: str, kind: str, championship: str
    ) -> subprocess.CompletedProcess[str]:
        section = self.text.split("- name: Enforce supported replay boundary", 1)[1]
        script = section.split("<<'PY'\n", 1)[1].split("\n          PY", 1)[0]
        lines = [
            line[10:] if line.startswith("          ") else line
            for line in script.splitlines()
        ]
        with tempfile.TemporaryDirectory() as directory:
            evidence = Path(directory) / "results.csv"
            fields = [
                "date", "sex", "season", "race", "event_kind", "race_type",
                "championship",
            ]
            with evidence.open("w", newline="", encoding="utf-8") as handle:
                writer = csv.DictWriter(handle, fieldnames=fields)
                writer.writeheader()
                writer.writerow({
                    "date": "2026-03-01", "sex": sex, "season": "2026",
                    "race": "1", "event_kind": kind,
                    "race_type": (
                        "Team Sprint" if kind == "team_sprint" else "Individual"
                    ),
                    "championship": championship,
                })
            return subprocess.run(
                [sys.executable, "-c", "\n".join(lines), str(evidence)],
                text=True, capture_output=True, check=False,
            )

    def test_boundary_execution_accepts_both_proven_event_shapes(self) -> None:
        for sex, kind in (
            ("M", "individual"),
            ("L", "individual"),
            ("Mixed", "team_sprint"),
        ):
            with self.subTest(sex=sex, kind=kind):
                result = self._run_boundary(sex=sex, kind=kind, championship="0")
                self.assertEqual(result.returncode, 0, result.stderr)

    def test_boundary_execution_rejects_championships_and_same_sex_teams(self) -> None:
        for sex, kind, championship in (
            ("M", "individual", "1"),
            ("M", "team", "0"),
        ):
            with self.subTest(sex=sex, kind=kind, championship=championship):
                result = self._run_boundary(
                    sex=sex, kind=kind, championship=championship
                )
                self.assertNotEqual(result.returncode, 0)
                self.assertIn(
                    "unsupported Nordic-combined hosted replay event", result.stderr
                )

    def test_unsupported_event_types_fail_closed_with_retained_evidence(self) -> None:
        self.assertIn("if not (regular_individual or regular_mixed_team_sprint):", self.text)
        self.assertIn("unsupported Nordic-combined hosted replay event", self.text)
        self.assertIn("championship={event[6]}", self.text)
        self.assertIn("if: always()", self.text)
        self.assertIn("retention-days: 30", self.text)

    def test_no_gap_run_succeeds_and_mutation_is_conditional(self) -> None:
        marker = "if: steps.gap.outputs.count != '0'"
        self.assertGreaterEqual(self.text.count(marker), 7)
        self.assertNotIn("--fail-on-gaps", self.text)

    def test_only_atomic_histories_and_receipt_are_staged(self) -> None:
        staged = self.text.split("git -C external/ski add", 1)[1].split(
            "if ! git", 1
        )[0]
        for name in ("men_chrono.csv", "ladies_chrono.csv", "team_sprint_chrono.csv"):
            self.assertIn(name, staged)
        self.assertIn("results_promotion_receipts", staged)
        self.assertNotIn("excel365/races.csv", staged)
        self.assertNotIn("static/", staged)

    def test_no_site_publication_or_season_close_is_claimed(self) -> None:
        self.assertNotIn("hugo", self.text.casefold())
        self.assertNotIn("season_close", self.text)


if __name__ == "__main__":
    unittest.main()
