from __future__ import annotations

import hashlib
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from openpyxl import Workbook

from pipelines.nordic_combined_prediction_publication import (
    NordicCombinedPublicationError,
    prepare_publication,
)
from pipelines.nordic_combined_prediction_shadow import (
    INDIVIDUAL_COLUMNS,
    TEAM_COLUMNS,
)
from pipelines.nordic_combined_prediction_shadow import (
    SCHEMA_VERSION as SHADOW_SCHEMA,
)

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-prediction-shadow.yml"


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_workbook(
    path: Path, sheets: list[tuple[str, tuple[str, ...], list[tuple[object, ...]]]]
) -> dict[str, dict[str, float | int]]:
    path.parent.mkdir(parents=True, exist_ok=True)
    workbook = Workbook()
    workbook.remove(workbook.active)
    counts: dict[str, dict[str, float | int]] = {}
    for name, headers, rows in sheets:
        sheet = workbook.create_sheet(name)
        sheet.append(headers)
        for row in rows:
            sheet.append(row)
        win_index = headers.index("Win")
        counts[name] = {
            "rows": len(rows),
            "win_total": sum(float(row[win_index]) for row in rows),
        }
    workbook.save(path)
    return counts


def individual_rows() -> list[tuple[object, ...]]:
    return [
        ("Jumper One", 1, "NOR", "M", 100, 50, 75, 90, 100, 100),
        ("Jumper Two", 2, "AUT", "M", 100, 50, 75, 90, 100, 100),
    ]


def team_rows(sex: str, race_type: str) -> list[tuple[object, ...]]:
    return [
        ("NOR", sex, race_type, 50, 75, 90, 100, 100),
        ("AUT", sex, race_type, 50, 75, 90, 100, 100),
    ]


def build_evidence(root: Path, *, logical_date: str = "2026-11-25") -> dict[str, Path]:
    run_root = root / "shadow/runs/test-run"
    reports_root = run_root / "reports"
    workbook_root = run_root / "workbooks" / logical_date.replace("-", "")
    normalization_path = root / "shadow/startlists/normalized/report.json"
    reports_root.mkdir(parents=True)
    normalization_path.parent.mkdir(parents=True)

    outputs = []
    workbook_specs = {
        "men_position_probabilities.xlsx": [
            ("Men Race 1", INDIVIDUAL_COLUMNS, individual_rows()),
            ("Men Race 2", INDIVIDUAL_COLUMNS, individual_rows()),
        ],
        "men_team_position_probabilities.xlsx": [
            ("Men Team", TEAM_COLUMNS, team_rows("M", "Team Large"))
        ],
        "men_team_sprint_position_probabilities.xlsx": [
            ("Men Team Sprint", TEAM_COLUMNS, team_rows("M", "Team Sprint"))
        ],
        "mixed_team_position_probabilities.xlsx": [
            ("Mixed Team", TEAM_COLUMNS, team_rows("Mixed", "Mixed Team"))
        ],
    }
    for filename, sheets in workbook_specs.items():
        path = workbook_root / filename
        counts = write_workbook(path, sheets)
        outputs.append({"file": filename, "sha256": sha256(path), "sheets": counts})

    plan = {
        "status": "ready_for_shadow_implementation",
        "logical_date": logical_date,
        "categories": {
            "men": 2,
            "ladies": 0,
            "men_team": 1,
            "ladies_team": 0,
            "men_team_sprint": 1,
            "ladies_team_sprint": 0,
            "mixed_team": 1,
            "mixed_team_sprint": 0,
        },
    }
    plan_path = reports_root / "plan.json"
    plan_path.write_text(json.dumps(plan), encoding="utf-8")
    normalization = {
        "schema_version": "nordicnumbers.nordic-combined-startlist-normalization/v1",
        "logical_date": logical_date,
        "status": "ready",
        "prediction_eligible": True,
        "accepted_startlists_modified": False,
    }
    normalization_path.write_text(json.dumps(normalization), encoding="utf-8")
    shadow = {
        "schema_version": SHADOW_SCHEMA,
        "pipeline_id": "nordic_combined_prediction_shadow",
        "run_id": "test-run",
        "logical_date": logical_date,
        "status": "passed",
        "code": {"blog_commit": "a" * 40, "ski_commit": "b" * 40},
        "publication": {"status": "disabled", "reason": "shadow only"},
        "plan": "reports/plan.json",
        "startlist_source": "normalized_official_fis",
        "startlist_normalization_report": {
            "file": str(normalization_path),
            "sha256": sha256(normalization_path),
        },
        "outputs": {"workbooks": outputs, "json": []},
    }
    shadow_path = reports_root / "shadow-report.json"
    shadow_path.write_text(json.dumps(shadow), encoding="utf-8")
    return {
        "shadow": shadow_path,
        "normalization": normalization_path,
        "workbooks": workbook_root,
    }


class NordicCombinedPredictionPublicationTests(unittest.TestCase):
    def test_prepares_individual_team_and_mixed_publication_with_parity(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence = build_evidence(root)
            repository = root / "repository"
            repository.mkdir()
            report_path = root / "publication-report.json"
            report = prepare_publication(
                shadow_report_path=evidence["shadow"],
                normalization_report_path=evidence["normalization"],
                workbook_root=evidence["workbooks"],
                repository_root=repository,
                report_path=report_path,
                evaluation_date=date(2026, 11, 25),
            )
            data_root = repository / "data/nordic-combined/drafts/race-picks/20261125"
            post = repository / "race-picks/fragments/20261125/nordic-combined.md"
            rendered = post.read_text(encoding="utf-8")

            self.assertEqual("prepared", report["status"])
            self.assertEqual(
                {
                    "workbook_hashes": "passed",
                    "sheet_parity": "passed",
                    "json_semantics": "passed",
                    "official_startlists": "passed",
                },
                report["quality"],
            )
            self.assertEqual(5, len(report["artifacts"]))
            self.assertEqual(
                {
                    "men_position_probabilities_Men_Race_1.json",
                    "men_position_probabilities_Men_Race_2.json",
                    "men_team_position_probabilities.json",
                    "men_team_sprint_position_probabilities.json",
                    "mixed_team_position_probabilities.json",
                },
                {path.name for path in data_root.glob("*.json")},
            )
            self.assertIn("### Men", rendered)
            self.assertIn("### Men Team", rendered)
            self.assertIn("### Men Team Sprint", rendered)
            self.assertIn("### Mixed Team", rendered)
            self.assertNotIn("ladies_position_probabilities", rendered)
            document = json.loads(
                (data_root / "men_position_probabilities_Men_Race_1.json").read_text()
            )
            self.assertEqual("Men Race 1", document["sheet_name"])
            self.assertEqual(2, len(document["rows"]))

            rerun = prepare_publication(
                shadow_report_path=evidence["shadow"],
                normalization_report_path=evidence["normalization"],
                workbook_root=evidence["workbooks"],
                repository_root=repository,
                report_path=report_path,
                evaluation_date=date(2026, 11, 25),
            )
            self.assertEqual("unchanged", rerun["status"])

    def test_workbook_tampering_blocks_before_repository_writes(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence = build_evidence(root)
            workbook = evidence["workbooks"] / "men_position_probabilities.xlsx"
            with workbook.open("ab") as handle:
                handle.write(b"tampered")
            repository = root / "repository"
            repository.mkdir()

            with self.assertRaisesRegex(NordicCombinedPublicationError, "hash mismatch"):
                prepare_publication(
                    shadow_report_path=evidence["shadow"],
                    normalization_report_path=evidence["normalization"],
                    workbook_root=evidence["workbooks"],
                    repository_root=repository,
                    report_path=root / "report.json",
                    evaluation_date=date(2026, 11, 25),
                )
            self.assertEqual([], list(repository.rglob("*")))

    def test_shadow_sheet_evidence_mismatch_blocks_publication(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence = build_evidence(root)
            shadow = json.loads(evidence["shadow"].read_text(encoding="utf-8"))
            shadow["outputs"]["workbooks"][0]["sheets"]["Men Race 1"]["rows"] = 999
            evidence["shadow"].write_text(json.dumps(shadow), encoding="utf-8")
            repository = root / "repository"
            repository.mkdir()

            with self.assertRaisesRegex(
                NordicCombinedPublicationError, "workbook sheet parity failed"
            ):
                prepare_publication(
                    shadow_report_path=evidence["shadow"],
                    normalization_report_path=evidence["normalization"],
                    workbook_root=evidence["workbooks"],
                    repository_root=repository,
                    report_path=root / "report.json",
                    evaluation_date=date(2026, 11, 25),
                )
            self.assertEqual([], list(repository.rglob("*")))

    def test_past_shadow_cannot_publish(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            evidence = build_evidence(root, logical_date="2026-11-24")
            with self.assertRaisesRegex(NordicCombinedPublicationError, "past prediction"):
                prepare_publication(
                    shadow_report_path=evidence["shadow"],
                    normalization_report_path=evidence["normalization"],
                    workbook_root=evidence["workbooks"],
                    repository_root=root / "repository",
                    report_path=root / "report.json",
                    evaluation_date=date(2026, 11, 25),
                )

    def test_workflow_automates_guarded_publication_after_site_data(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        site_data_source = (
            ROOT / ".github/workflows/nordic-combined-site-data-update.yml"
        ).read_text(encoding="utf-8")
        self.assertIn(
            "gh workflow run nordic-combined-prediction-shadow.yml", site_data_source
        )
        self.assertIn("-f publish_predictions=true", site_data_source)
        self.assertIn("pipelines.nordic_combined_prediction_publication", source)
        self.assertIn("contents: write", source)
        self.assertIn("gh workflow run combined-race-picks.yml", source)
        self.assertIn("actions: write", source)
        self.assertIn("git pull --rebase origin main", source)
        self.assertIn("race-picks/fragments/$date_key/nordic-combined.md", source)
        self.assertIn("inputs.publish_predictions == true", source)


if __name__ == "__main__":
    unittest.main()

