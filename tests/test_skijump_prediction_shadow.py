from __future__ import annotations

import csv
import json
from pathlib import Path
import re
import subprocess
import tempfile
import unittest
import zipfile

from openpyxl import Workbook

from pipelines.skijump_prediction_shadow import (
    INDIVIDUAL_COLUMNS,
    PredictionShadowError,
    run_shadow,
)


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/ski-jumping-prediction-shadow.yml"
SCHEDULE_FIELDS = (
    "Date", "Startlist", "Sex", "City", "Country", "HillSize", "RaceType",
    "Championship", "Race",
)


def write_csv(path: Path, fields: tuple[str, ...], rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def build_runtime(root: Path, *, schedule_date: str, startlist_date: str) -> tuple[Path, Path]:
    blog = root / "blog"
    ski = root / "ski"
    model = blog / "content/post/skijump/drafts/race-picks-simulation.R"
    model.parent.mkdir(parents=True)
    model.write_text("# mocked by the unit test\n", encoding="utf-8")
    input_root = ski / "elo/python/skijump/polars/excel365"
    write_csv(
        input_root / "races.csv",
        SCHEDULE_FIELDS,
        [{
            "Date": schedule_date,
            "Startlist": "https://fis.example/startlist",
            "Sex": "M",
            "City": "Planica",
            "Country": "Slovenia",
            "HillSize": "240",
            "RaceType": "Flying",
            "Championship": "0",
            "Race": "1",
        }],
    )
    (input_root / "men_chrono_elevation.csv").write_text(
        "ID,Date,Place\n1,2026-03-01,1\n", encoding="utf-8"
    )
    write_csv(
        input_root / "startlist_races_men.csv",
        ("ID", "Skier", "Nation", "Race_Date"),
        [{"ID": "1", "Skier": "Example Jumper", "Nation": "NOR", "Race_Date": startlist_date}],
    )
    return blog, ski


def write_individual_workbook(path: Path, *, invalid_probability: bool = False) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    workbook = Workbook()
    sheet = workbook.active
    sheet.title = "Men Race 1"
    sheet.append(INDIVIDUAL_COLUMNS)
    sheet.append(
        ("Example Jumper", 1, "NOR", "M", 100, 101 if invalid_probability else 50, 75, 90, 100, 100)
    )
    sheet.append(
        ("Second Jumper", 2, "SLO", "M", 100, 50, 75, 90, 100, 100)
    )
    workbook.save(path)
    # Match the underspecified worksheet dimension emitted by openxlsx.
    rewritten = path.with_suffix(".rewritten.xlsx")
    with zipfile.ZipFile(path) as source, zipfile.ZipFile(rewritten, "w") as target:
        for member in source.infolist():
            data = source.read(member.filename)
            if member.filename == "xl/worksheets/sheet1.xml":
                data = re.sub(
                    br'<dimension ref="[^"]+"/>',
                    b'<dimension ref="A1"/>',
                    data,
                    count=1,
                )
            target.writestr(member, data)
    rewritten.replace(path)


class SkiJumpingPredictionShadowTests(unittest.TestCase):
    def test_successful_shadow_pins_inputs_and_validates_workbook(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/29/2026"
            )
            captured: dict[str, str] = {}

            def fake_runner(command, **kwargs):
                environment = kwargs["env"]
                captured.update(environment)
                output = Path(environment["NORDIC_OUTPUT_ROOT"]) / "20260329"
                write_individual_workbook(output / "men_position_probabilities.xlsx")
                return subprocess.CompletedProcess(command, 0)

            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date="2026-03-29",
                run_id="test-run",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                command_runner=fake_runner,
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))

        self.assertEqual("passed", report["status"])
        self.assertEqual({"status": "disabled", "reason": "shadow runs retain evidence and cannot create or publish posts"}, report["publication"])
        self.assertEqual(3, len(report["inputs"]))
        self.assertEqual(1, report["startlist_rows"]["men"])
        self.assertEqual("42", captured["NORDIC_RANDOM_SEED"])
        self.assertEqual("25", captured["NORDIC_N_SIMULATIONS"])
        self.assertEqual("false", captured["NORDIC_LOAD_ENV"])
        self.assertEqual({"Men Race 1": 2}, report["outputs"][0]["sheets"])
        self.assertEqual("logs/simulation-process.log", report["evidence"]["process_log"])

    def test_no_race_is_a_successful_no_op(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/29/2026"
            )

            def forbidden_runner(*args, **kwargs):
                raise AssertionError("R must not run on a no-race date")

            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date="2026-03-30",
                run_id="no-race",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                command_runner=forbidden_runner,
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))

        self.assertEqual("skipped", report["status"])
        self.assertEqual("disabled", report["publication"]["status"])

    def test_stale_startlist_fails_before_simulation_and_retains_report(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/28/2026"
            )
            with self.assertRaises(PredictionShadowError):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date="2026-03-29",
                    run_id="stale-startlist",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    command_runner=lambda *args, **kwargs: self.fail("R unexpectedly ran"),
                )
            report = json.loads(
                (root / "output/runs/stale-startlist/reports/shadow-report.json").read_text()
            )

        self.assertEqual("failed", report["status"])
        self.assertIn("does not match", report["error"])
        self.assertEqual("disabled", report["publication"]["status"])

    def test_invalid_probability_fails_workbook_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/29/2026"
            )

            def fake_runner(command, **kwargs):
                output = Path(kwargs["env"]["NORDIC_OUTPUT_ROOT"]) / "20260329"
                write_individual_workbook(
                    output / "men_position_probabilities.xlsx",
                    invalid_probability=True,
                )
                return subprocess.CompletedProcess(command, 0)

            with self.assertRaises(PredictionShadowError):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date="2026-03-29",
                    run_id="bad-workbook",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    command_runner=fake_runner,
                )

    def test_workflow_is_manual_read_only_pinned_and_retains_evidence(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        trigger = source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertNotIn("workflow_run:", trigger)
        self.assertIn("contents: read", source)
        self.assertNotIn("contents: write", source)
        self.assertIn('repos/syverjohansen/ski/commits/$SKI_REF', source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", source)
        self.assertIn("-m pipelines.skijump_prediction_shadow", source)
        self.assertIn("continue-on-error: true", source)
        self.assertIn("if: always()", source)
        self.assertIn("retention-days: 30", source)
        self.assertIn('"status": "disabled"', source)
        self.assertNotIn("git push", source)
        self.assertNotIn("hugo", source.lower())


if __name__ == "__main__":
    unittest.main()
