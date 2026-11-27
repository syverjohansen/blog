from __future__ import annotations

import hashlib
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.alpine_publication_candidate import prepare_candidate
from pipelines.test_pipeline import PREDICTION_HEADERS, PipelineCheckError


class AlpinePublicationCandidateTests(unittest.TestCase):
    def build_evidence(self, root: Path) -> dict[str, Path]:
        site_data = root / "artifact/workspace/site-data"
        generated = (
            site_data
            / "alpine/drafts/race-picks/20261121/ladies_position_probabilities.json"
        )
        generated.parent.mkdir(parents=True)
        document = {
            "sheet_name": "Ladies Race 1",
            "headers": list(PREDICTION_HEADERS),
            "rows": [["Skier One", 1, "NOR", "L", 100, 10, 20, 30, 40, 50]],
        }
        generated.write_text(json.dumps(document), encoding="utf-8")
        digest = hashlib.sha256(generated.read_bytes()).hexdigest()
        manifest = root / "artifact/manifests/alpine-run-manifest-final.json"
        manifest.parent.mkdir(parents=True)
        manifest.write_text(
            json.dumps(
                {
                    "schema_version": "nordicnumbers.alpine-production-shadow/v1",
                    "pipeline_id": "alpine_production_shadow",
                    "run_id": "test-run",
                    "logical_date": "2026-11-21",
                    "status": "passed",
                    "code": {
                        "blog": {"commit_sha": "a" * 40, "working_tree_dirty": False},
                        "ski": {"commit_sha": "b" * 40, "working_tree_dirty": False},
                    },
                    "task_outcomes": [{"task_id": "run", "status": "passed"}],
                    "outputs": [
                        {
                            "file": generated.name,
                            "rows": 1,
                            "sha256": digest,
                            "semantic_contract": "passed",
                        }
                    ],
                    "quality_results": {
                        "schema": "passed",
                        "probability_bounds": "passed",
                        "probability_monotonicity": "passed",
                        "hugo_render": {"status": "passed"},
                        "current_input_readiness": {
                            "status": "passed",
                            "genders": ["ladies"],
                        },
                    },
                    "publication": {"status": "disabled"},
                }
            ),
            encoding="utf-8",
        )
        replay = root / "artifact/acquisition/reports/replay.json"
        replay.parent.mkdir(parents=True)
        replay.write_text(
            json.dumps(
                {
                    "status": "passed",
                    "logical_date": "2026-11-21",
                    "publication": {"status": "disabled"},
                    "races": [
                        {
                            "source": "generated_pre_race_history",
                            "result_fields_in_prediction_input": False,
                        }
                    ],
                }
            ),
            encoding="utf-8",
        )
        actual = root / "artifact/acquisition/reports/actual-results.json"
        actual.write_text("[]\n", encoding="utf-8")
        return {
            "manifest": manifest,
            "replay": replay,
            "actual": actual,
            "site_data": site_data,
        }

    def prepare(self, root: Path) -> dict[str, object]:
        evidence = self.build_evidence(root)
        return prepare_candidate(
            manifest_path=evidence["manifest"],
            replay_report_path=evidence["replay"],
            actual_results_path=evidence["actual"],
            site_data_root=evidence["site_data"],
            repository_root=root / "repository",
            report_path=root / "candidate-report.json",
            evaluation_date=date(2026, 11, 17),
        )

    def test_prepares_only_validated_json_and_reviewable_content(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-") as text:
            root = Path(text)
            report = self.prepare(root)
            self.assertEqual("prepared", report["status"])
            self.assertEqual("ready", report["publication"]["status"])
            data = (
                root
                / "repository/data/alpine/drafts/race-picks/20261121"
                / "ladies_position_probabilities.json"
            )
            content = root / "repository/data/race-picks/fragments/20261121/alpine.md"
            self.assertTrue(data.is_file())
            rendered = content.read_text()
            self.assertIn("Race Picks for 11/21/2026", rendered)
            self.assertIn(
                '{{< alpine/datatable "alpine/drafts/race-picks/20261121/'
                'ladies_position_probabilities" >}}',
                rendered,
            )
            self.assertNotIn('ladies_position_probabilities" >}\n', rendered)

    def test_rejects_completed_results_before_writing_repository(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-results-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            evidence["actual"].write_text(
                json.dumps(
                    [
                        {
                            "Rank": "1",
                            "Time": "1:42.33",
                            "Run1": "51.00",
                            "Run2": "51.33",
                            "Points": "0.00",
                        }
                    ]
                ),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(PipelineCheckError, "completed results"):
                prepare_candidate(
                    manifest_path=evidence["manifest"],
                    replay_report_path=evidence["replay"],
                    actual_results_path=evidence["actual"],
                    site_data_root=evidence["site_data"],
                    repository_root=root / "repository",
                    report_path=root / "candidate-report.json",
                    evaluation_date=date(2026, 11, 17),
                )
            self.assertFalse((root / "repository").exists())

    def test_accepts_blank_rows_from_validated_historical_proxy(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-proxy-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            replay = json.loads(evidence["replay"].read_text())
            replay["races"][0]["source"] = "historical_result_proxy"
            evidence["replay"].write_text(json.dumps(replay), encoding="utf-8")
            evidence["actual"].write_text(
                json.dumps(
                    [
                        {
                            "Rank": "1",
                            "Name": "Skier One",
                            "ID": "100",
                            "Time": "",
                            "Run1": "",
                            "Run2": "",
                            "Points": "",
                        },
                        {
                            "Rank": "2",
                            "Name": "Skier Two",
                            "ID": "200",
                            "Time": None,
                            "Run1": " ",
                            "Run2": "",
                            "Points": "",
                        },
                    ]
                ),
                encoding="utf-8",
            )

            report = prepare_candidate(
                manifest_path=evidence["manifest"],
                replay_report_path=evidence["replay"],
                actual_results_path=evidence["actual"],
                site_data_root=evidence["site_data"],
                repository_root=root / "repository",
                report_path=root / "candidate-report.json",
                evaluation_date=date(2026, 11, 17),
            )

            self.assertEqual("prepared", report["status"])

    def test_rejects_blank_rows_without_validated_proxy_source(self) -> None:
        with tempfile.TemporaryDirectory(
            prefix="alpine-publication-ambiguous-"
        ) as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            evidence["actual"].write_text(
                json.dumps(
                    [
                        {
                            "Rank": "1",
                            "Time": "",
                            "Run1": "",
                            "Run2": "",
                            "Points": "",
                        }
                    ]
                ),
                encoding="utf-8",
            )

            with self.assertRaisesRegex(
                PipelineCheckError, "not a validated historical result proxy"
            ):
                prepare_candidate(
                    manifest_path=evidence["manifest"],
                    replay_report_path=evidence["replay"],
                    actual_results_path=evidence["actual"],
                    site_data_root=evidence["site_data"],
                    repository_root=root / "repository",
                    report_path=root / "candidate-report.json",
                    evaluation_date=date(2026, 11, 17),
                )
            self.assertFalse((root / "repository").exists())

    def test_rejects_dirty_source_provenance(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-dirty-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            manifest = json.loads(evidence["manifest"].read_text())
            manifest["code"]["blog"]["working_tree_dirty"] = True
            evidence["manifest"].write_text(json.dumps(manifest), encoding="utf-8")
            with self.assertRaisesRegex(PipelineCheckError, "provenance is not clean"):
                prepare_candidate(
                    manifest_path=evidence["manifest"],
                    replay_report_path=evidence["replay"],
                    actual_results_path=evidence["actual"],
                    site_data_root=evidence["site_data"],
                    repository_root=root / "repository",
                    report_path=root / "candidate-report.json",
                    evaluation_date=date(2026, 11, 17),
                )

    def test_refuses_to_overwrite_different_published_content(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-conflict-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            target = (
                root
                / "repository/data/alpine/drafts/race-picks/20261121"
                / "ladies_position_probabilities.json"
            )
            target.parent.mkdir(parents=True)
            target.write_text("different", encoding="utf-8")
            with self.assertRaisesRegex(PipelineCheckError, "different content"):
                prepare_candidate(
                    manifest_path=evidence["manifest"],
                    replay_report_path=evidence["replay"],
                    actual_results_path=evidence["actual"],
                    site_data_root=evidence["site_data"],
                    repository_root=root / "repository",
                    report_path=root / "candidate-report.json",
                    evaluation_date=date(2026, 11, 17),
                )

    def test_identical_retry_is_unchanged(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-retry-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            arguments = {
                "manifest_path": evidence["manifest"],
                "replay_report_path": evidence["replay"],
                "actual_results_path": evidence["actual"],
                "site_data_root": evidence["site_data"],
                "repository_root": root / "repository",
                "report_path": root / "candidate-report.json",
                "evaluation_date": date(2026, 11, 17),
            }

            first = prepare_candidate(**arguments)
            second = prepare_candidate(**arguments)

            self.assertEqual("prepared", first["status"])
            self.assertEqual("unchanged", second["status"])
            self.assertTrue(
                all(item["status"] == "unchanged" for item in second["files"])
            )

    def test_rejects_past_logical_date(self) -> None:
        with tempfile.TemporaryDirectory(prefix="alpine-publication-past-") as text:
            root = Path(text)
            evidence = self.build_evidence(root)
            with self.assertRaisesRegex(PipelineCheckError, "past logical date"):
                prepare_candidate(
                    manifest_path=evidence["manifest"],
                    replay_report_path=evidence["replay"],
                    actual_results_path=evidence["actual"],
                    site_data_root=evidence["site_data"],
                    repository_root=root / "repository",
                    report_path=root / "candidate-report.json",
                    evaluation_date=date(2026, 11, 22),
                )


if __name__ == "__main__":
    unittest.main()
