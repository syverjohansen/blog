from __future__ import annotations

import csv
import hashlib
import json
import re
import subprocess
import tempfile
import unittest
import zipfile
from pathlib import Path

from openpyxl import Workbook

from pipelines.skijump_prediction_shadow import (
    INDIVIDUAL_COLUMNS,
    PredictionShadowError,
    run_shadow,
)

ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/ski-jumping-prediction-shadow.yml"
SCHEDULE_FIELDS = (
    "Date",
    "Startlist",
    "Sex",
    "City",
    "Country",
    "HillSize",
    "RaceType",
    "Championship",
    "Race",
)


def write_csv(
    path: Path, fields: tuple[str, ...], rows: list[dict[str, object]]
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def build_runtime(
    root: Path, *, schedule_date: str, startlist_date: str
) -> tuple[Path, Path]:
    blog = root / "blog"
    ski = root / "ski"
    model = blog / "content/post/skijump/drafts/race-picks-simulation.R"
    model.parent.mkdir(parents=True)
    model.write_text("# mocked by the unit test\n", encoding="utf-8")
    input_root = ski / "elo/python/skijump/polars/excel365"
    write_csv(
        input_root / "races.csv",
        SCHEDULE_FIELDS,
        [
            {
                "Date": schedule_date,
                "Startlist": "https://fis.example/startlist",
                "Sex": "M",
                "City": "Planica",
                "Country": "Slovenia",
                "HillSize": "240",
                "RaceType": "Flying",
                "Championship": "0",
                "Race": "1",
            }
        ],
    )
    (input_root / "men_chrono_elevation.csv").write_text(
        "ID,Date,Place\n1,2026-03-01,1\n", encoding="utf-8"
    )
    write_csv(
        input_root / "startlist_races_men.csv",
        ("ID", "Skier", "Nation", "Race_Date"),
        [
            {
                "ID": "1",
                "Skier": "Example Jumper",
                "Nation": "NOR",
                "Race_Date": startlist_date,
            }
        ],
    )
    return blog, ski


def write_individual_workbook(path: Path, *, invalid_probability: bool = False) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    workbook = Workbook()
    sheet = workbook.active
    sheet.title = "Men Race 1"
    sheet.append(INDIVIDUAL_COLUMNS)
    sheet.append(
        (
            "Example Jumper",
            1,
            "NOR",
            "M",
            100,
            101 if invalid_probability else 50,
            75,
            90,
            100,
            100,
        )
    )
    sheet.append(("Second Jumper", 2, "SLO", "M", 100, 50, 75, 90, 100, 100))
    workbook.save(path)
    # Match the underspecified worksheet dimension emitted by openxlsx.
    rewritten = path.with_suffix(".rewritten.xlsx")
    with zipfile.ZipFile(path) as source, zipfile.ZipFile(rewritten, "w") as target:
        for member in source.infolist():
            data = source.read(member.filename)
            if member.filename == "xl/worksheets/sheet1.xml":
                data = re.sub(
                    rb'<dimension ref="[^"]+"/>',
                    b'<dimension ref="A1"/>',
                    data,
                    count=1,
                )
            target.writestr(member, data)
    rewritten.replace(path)


def build_normalized_startlist(root: Path, *, logical_date: str) -> tuple[Path, Path]:
    normalization = root / "normalization"
    candidate_root = normalization / "candidates"
    candidate = candidate_root / "startlist_races_men.csv"
    write_csv(
        candidate,
        ("ID", "Skier", "Nation", "Race_Date"),
        [
            {
                "ID": "1",
                "Skier": "Example Jumper",
                "Nation": "NOR",
                "Race_Date": logical_date,
            }
        ],
    )
    report = {
        "schema_version": "nordicnumbers.skijump-startlist-normalization/v1",
        "logical_date": logical_date,
        "status": "ready",
        "prediction_eligible": True,
        "accepted_startlists_modified": False,
        "outputs": {
            "men": {
                "file": "candidates/startlist_races_men.csv",
                "rows": 1,
                "sha256": hashlib.sha256(candidate.read_bytes()).hexdigest(),
            }
        },
    }
    report_path = normalization / "report.json"
    report_path.write_text(json.dumps(report), encoding="utf-8")
    return candidate_root, report_path


class SkiJumpingPredictionShadowTests(unittest.TestCase):
    def test_generated_startlists_are_verified_and_isolated_for_the_model(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/28/2026"
            )
            candidate_root, normalization_report = build_normalized_startlist(
                root, logical_date="2026-03-29"
            )
            captured: dict[str, str] = {}

            def fake_runner(command, **kwargs):
                environment = kwargs["env"]
                captured.update(environment)
                model_input = Path(environment["NORDIC_INPUT_ROOT"])
                self.assertNotEqual(
                    ski / "elo/python/skijump/polars/excel365", model_input
                )
                self.assertEqual(
                    candidate_root.joinpath("startlist_races_men.csv").read_bytes(),
                    model_input.joinpath("startlist_races_men.csv").read_bytes(),
                )
                self.assertTrue(model_input.joinpath("races.csv").is_file())
                output = Path(environment["NORDIC_OUTPUT_ROOT"]) / "20260329"
                write_individual_workbook(output / "men_position_probabilities.xlsx")
                return subprocess.CompletedProcess(command, 0)

            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date="2026-03-29",
                run_id="generated-startlist",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                startlist_root=candidate_root,
                startlist_report=normalization_report,
                command_runner=fake_runner,
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))

        self.assertEqual("passed", report["status"])
        self.assertEqual("normalized_official_fis", report["startlist_source"])
        self.assertEqual(1, report["startlist_rows"]["men"])
        generated = [
            item
            for item in report["inputs"]
            if item.get("source") == "normalized_official_fis"
        ]
        self.assertEqual(["men"], [item["category"] for item in generated])
        self.assertIn("model-inputs/individual", captured["NORDIC_INPUT_ROOT"])

    def test_generated_startlist_checksum_drift_blocks_simulation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(
                root, schedule_date="03/29/2026", startlist_date="03/28/2026"
            )
            candidate_root, normalization_report = build_normalized_startlist(
                root, logical_date="2026-03-29"
            )
            with (candidate_root / "startlist_races_men.csv").open(
                "a", encoding="utf-8"
            ) as handle:
                handle.write("2,Tampered Jumper,SLO,2026-03-29\n")

            with self.assertRaisesRegex(PredictionShadowError, "checksum mismatch"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date="2026-03-29",
                    run_id="tampered-startlist",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    startlist_root=candidate_root,
                    startlist_report=normalization_report,
                    command_runner=lambda *args, **kwargs: self.fail(
                        "R unexpectedly ran"
                    ),
                )

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
        self.assertEqual(
            {
                "status": "disabled",
                "reason": "shadow runs retain evidence and cannot create or publish posts",
            },
            report["publication"],
        )
        self.assertEqual(3, len(report["inputs"]))
        self.assertEqual(1, report["startlist_rows"]["men"])
        self.assertEqual("42", captured["NORDIC_RANDOM_SEED"])
        self.assertEqual("25", captured["NORDIC_N_SIMULATIONS"])
        self.assertEqual("false", captured["NORDIC_LOAD_ENV"])
        self.assertEqual({"Men Race 1": 2}, report["outputs"][0]["sheets"])
        self.assertEqual(
            "logs/simulation-process.log", report["evidence"]["process_log"]
        )

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
                    command_runner=lambda *args, **kwargs: self.fail(
                        "R unexpectedly ran"
                    ),
                )
            report = json.loads(
                (
                    root / "output/runs/stale-startlist/reports/shadow-report.json"
                ).read_text()
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

    def test_shadow_job_is_read_only_pinned_and_retains_evidence(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        trigger = source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertIn("schedule:", trigger)
        self.assertIn('cron: "17 5,11,17,23 * 1-3,11-12 *"', trigger)
        self.assertNotIn("workflow_run:", trigger)
        global_permissions = source.split("permissions:\n", 1)[1].split(
            "\n\nconcurrency:", 1
        )[0]
        self.assertIn("contents: read", global_permissions)
        shadow_job = source.split("  shadow:\n", 1)[1].split("\n  publish:\n", 1)[0]
        self.assertNotIn("contents: write", shadow_job)
        self.assertIn("repos/syverjohansen/ski/commits/$SKI_REF", source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", source)
        self.assertIn("-m pipelines.skijump_prediction_shadow", source)
        self.assertIn("skijump_startlist_acquire.py", source)
        self.assertIn("skijump_startlist_normalize.py", source)
        self.assertIn("/pipelines/requirements-snapshot.txt", source)
        self.assertIn('--startlist-root "$STARTLIST_CANDIDATE_ROOT"', source)
        self.assertIn('--startlist-report "$STARTLIST_NORMALIZATION_REPORT"', source)
        sparse_checkout = source.split("sparse-checkout: |", 2)[2].split(
            "\n\n      - name: Prepare", 1
        )[0]
        self.assertNotIn("startlist_races_men.csv", sparse_checkout)
        self.assertNotIn("startlist_team_races_men.csv", sparse_checkout)
        self.assertIn("continue-on-error: true", source)
        self.assertIn("if: always()", source)
        self.assertIn("retention-days: 30", source)
        self.assertIn('"status": "disabled"', source)
        self.assertNotIn("git push", shadow_job)
        self.assertNotIn("hugo", shadow_job.lower())


if __name__ == "__main__":
    unittest.main()
