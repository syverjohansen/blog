from __future__ import annotations

import json
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from pipelines.alpine_shadow_runner import ShadowRunnerError, run_shadow


class AlpineShadowRunnerTests(unittest.TestCase):
    def build_repositories(self, root: Path) -> tuple[Path, Path]:
        blog = root / "blog"
        ski = root / "ski"
        (blog / "pipelines").mkdir(parents=True)
        (blog / "pipelines/test_pipeline.py").write_text("# fixture\n")
        runtime = ski / "elo/python/alpine/polars"
        (runtime / "excel365").mkdir(parents=True)
        (runtime / "startlist-scrape-races.py").write_text("# fixture\n")
        for filename in (
            "races.csv",
            "men_chrono.csv",
            "ladies_chrono.csv",
            "fis_identity_aliases.csv",
        ):
            (runtime / "excel365" / filename).write_text("fixture\n")
        return blog, ski

    @staticmethod
    def git_state(root: Path) -> dict[str, object]:
        return {
            "commit_sha": f"{root.name}-sha",
            "working_tree_dirty": False,
        }

    def test_success_retains_workspace_and_enriches_manifest(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-shadow-runner-") as text:
            root = Path(text)
            blog, ski = self.build_repositories(root)
            output = root / "output"

            def command_runner(
                command: list[str], **kwargs: object
            ) -> subprocess.CompletedProcess[str]:
                handle = kwargs["stdout"]
                self.assertTrue(hasattr(handle, "write"))
                if command[1].endswith("startlist-scrape-races.py"):
                    acquisition = Path(command[command.index("--output-dir") + 1])
                    acquisition.mkdir(parents=True)
                    handle.write("acquisition passed\n")
                    return subprocess.CompletedProcess(command, 0)

                workspace = Path(command[command.index("--workspace-root") + 1])
                workspace.mkdir(parents=True)
                (workspace / "candidate.json").write_text("{}\n")
                manifest = Path(command[command.index("--manifest") + 1])
                manifest.parent.mkdir(parents=True)
                manifest.write_text(
                    json.dumps(
                        {
                            "schema_version": "nordicnumbers.pipeline-run/v1",
                            "pipeline_id": "alpine_current_season_smoke",
                            "run_id": "test-run",
                            "status": "passed",
                            "task_outcomes": [
                                {"task_id": "simulate_predictions", "status": "passed"}
                            ],
                            "publication": {"status": "disabled"},
                        }
                    )
                    + "\n"
                )
                handle.write("pipeline passed\n")
                return subprocess.CompletedProcess(command, 0)

            with patch(
                "pipelines.alpine_shadow_runner.git_state",
                side_effect=self.git_state,
            ):
                manifest_path = run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=output,
                    logical_date="2026-11-28",
                    expected_season=2027,
                    n_simulations=10,
                    run_id="test-run",
                    allow_network=False,
                    command_runner=command_runner,
                )

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertEqual("alpine_production_shadow", manifest["pipeline_id"])
            self.assertEqual("disabled", manifest["publication"]["status"])
            self.assertEqual(
                "schedule_gate",
                manifest["task_outcomes"][0]["task_id"],
            )
            self.assertEqual(
                "acquire_current_inputs",
                manifest["task_outcomes"][1]["task_id"],
            )
            self.assertTrue(
                (output / "runs/test-run/workspace/candidate.json").is_file()
            )
            self.assertEqual(2, len(manifest["evidence"]["logs"]))

    def test_acquisition_failure_retains_log_and_disabled_manifest(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-shadow-failure-") as text:
            root = Path(text)
            blog, ski = self.build_repositories(root)
            output = root / "output"

            def command_runner(
                command: list[str], **kwargs: object
            ) -> subprocess.CompletedProcess[str]:
                kwargs["stdout"].write("no Alpine races found\n")
                return subprocess.CompletedProcess(command, 1)

            with (
                patch(
                    "pipelines.alpine_shadow_runner.git_state",
                    side_effect=self.git_state,
                ),
                self.assertRaisesRegex(ShadowRunnerError, "acquisition failed"),
            ):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=output,
                    logical_date="2026-11-23",
                    expected_season=2027,
                    n_simulations=10,
                    run_id="failed-run",
                    allow_network=False,
                    command_runner=command_runner,
                )

            run_root = output / "runs/failed-run"
            manifest = json.loads(
                (run_root / "manifests/alpine-run-manifest-final.json").read_text()
            )
            self.assertEqual("failed", manifest["status"])
            self.assertEqual("disabled", manifest["publication"]["status"])
            self.assertEqual(
                ["passed", "failed", "skipped"],
                [outcome["status"] for outcome in manifest["task_outcomes"]],
            )
            self.assertIn(
                "no Alpine races found",
                (run_root / "logs/acquire-current-inputs.log").read_text(),
            )

    def test_scheduled_quiet_day_writes_skip_manifest_without_subprocesses(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-shadow-skip-") as text:
            root = Path(text)
            blog, ski = self.build_repositories(root)
            races = ski / "elo/python/alpine/polars/excel365/races.csv"
            races.write_text("Date,Sex\n11/28/2026,L\n", encoding="utf-8")
            output = root / "output"

            def unexpected_runner(
                command: list[str], **kwargs: object
            ) -> subprocess.CompletedProcess[str]:
                self.fail(f"quiet-day schedule invoked subprocess: {command}")

            with patch(
                "pipelines.alpine_shadow_runner.git_state",
                side_effect=self.git_state,
            ):
                manifest_path = run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=output,
                    logical_date="2026-11-23",
                    expected_season=2027,
                    n_simulations=10,
                    run_id="quiet-run",
                    allow_network=False,
                    skip_no_race=True,
                    command_runner=unexpected_runner,
                )

            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
            self.assertEqual("skipped", manifest["status"])
            self.assertEqual("disabled", manifest["publication"]["status"])
            self.assertIn("no Alpine race", manifest["skip_reason"])

    def test_output_root_inside_source_repository_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-shadow-isolation-") as text:
            root = Path(text)
            blog, ski = self.build_repositories(root)
            with self.assertRaisesRegex(ShadowRunnerError, "outside repository"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=blog / "artifacts",
                    logical_date="2026-11-28",
                    expected_season=2027,
                    n_simulations=10,
                    run_id="unsafe-run",
                    allow_network=False,
                )


if __name__ == "__main__":
    unittest.main()
