from __future__ import annotations

import csv
import hashlib
import json
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path

from openpyxl import Workbook, load_workbook

from pipelines.nordic_combined_prediction_contract import CATEGORY_CONTRACTS
from pipelines.nordic_combined_prediction_shadow import (
    INDIVIDUAL_COLUMNS,
    TEAM_COLUMNS,
    PredictionShadowError,
    run_shadow,
)


ROOT = Path(__file__).resolve().parents[1]
WORKFLOW = ROOT / ".github/workflows/nordic-combined-prediction-shadow.yml"
DATE = "2026-02-10"
DATE_KEY = "20260210"
SCHEDULE_FIELDS = (
    "Date", "Final_Date", "Startlist", "Sex", "City", "Country", "Distance",
    "RaceType", "Period", "Elevation", "Championship", "Race",
)
CATEGORIES = (
    ("men", "M", "Individual"),
    ("ladies", "L", "Individual Compact"),
    ("men_team", "M", "Team"),
    ("ladies_team", "L", "Team"),
    ("men_team_sprint", "M", "Team Sprint"),
    ("ladies_team_sprint", "L", "Team Sprint"),
    ("mixed_team", "Mixed", "Team"),
    ("mixed_team_sprint", "Mixed", "Team Sprint"),
)
NORMALIZATION_SCHEMA = "nordicnumbers.nordic-combined-startlist-normalization/v1"


def write_csv(path: Path, fields: tuple[str, ...], rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def build_runtime(root: Path, *, all_categories: bool = True) -> tuple[Path, Path]:
    blog = root / "blog"
    ski = root / "ski"
    model = blog / "content/post/nordic-combined/drafts/race-picks-simulation.R"
    converter = blog / "static/python/excel_to_hugo_multiple_sheets.py"
    model.parent.mkdir(parents=True)
    converter.parent.mkdir(parents=True)
    model.write_text("# mocked\n", encoding="utf-8")
    converter.write_text("# mocked\n", encoding="utf-8")

    rows = []
    selected = CATEGORIES if all_categories else CATEGORIES[:1]
    for race, (_, sex, race_type) in enumerate(selected, 1):
        rows.append(
            {
                "Date": "02/10/2026",
                "Final_Date": "02/10/2026",
                "Startlist": f"https://fis.example/{race}",
                "Sex": sex,
                "City": "Predazzo",
                "Country": "Italy",
                "Distance": "10",
                "RaceType": race_type,
                "Period": "4",
                "Elevation": "1000",
                "Championship": "1",
                "Race": race,
            }
        )
    individual = ski / "elo/python/nordic-combined/polars/excel365"
    team = ski / "elo/python/nordic-combined/polars/relay/excel365"
    write_csv(individual / "races.csv", SCHEDULE_FIELDS, rows)
    for name in ("men_chrono_elevation.csv", "ladies_chrono_elevation.csv"):
        write_csv(individual / name, ("ID", "Date", "Place"), [{"ID": 1, "Date": DATE, "Place": 1}])
    for name in ("men_team_chrono.csv", "ladies_team_chrono.csv", "mixed_team_chrono.csv", "team_sprint_chrono.csv"):
        write_csv(team / name, ("Nation", "Date", "Place"), [{"Nation": "NOR", "Date": DATE, "Place": 1}])

    for category, _, _ in selected:
        relative = next(
            str(value)
            for value in CATEGORY_CONTRACTS[category]["inputs"]
            if "startlist" in Path(str(value)).name
        )
        target = ski / relative
        if "_team" in category:
            write_csv(
                target,
                ("Nation", "Race_Date", "Member_1_ID"),
                [
                    {"Nation": "NOR", "Race_Date": DATE, "Member_1_ID": 1},
                    {"Nation": "GER", "Race_Date": DATE, "Member_1_ID": 2},
                ],
            )
        else:
            write_csv(
                target,
                ("ID", "Skier", "Nation", "Race_Date"),
                [
                    {"ID": 1, "Skier": "One", "Nation": "NOR", "Race_Date": DATE},
                    {"ID": 2, "Skier": "Two", "Nation": "GER", "Race_Date": DATE},
                ],
            )
    return blog, ski


def build_normalized_startlists(
    root: Path, ski: Path, *, all_categories: bool = True
) -> tuple[Path, Path]:
    normalization_root = root / "normalized"
    candidate_root = normalization_root / "candidates"
    candidate_root.mkdir(parents=True)
    selected = CATEGORIES if all_categories else CATEGORIES[:1]
    outputs: dict[str, dict[str, object]] = {}
    for category, _, _ in selected:
        relative = next(
            str(value)
            for value in CATEGORY_CONTRACTS[category]["inputs"]
            if "startlist" in Path(str(value)).name
        )
        source = ski / relative
        candidate = candidate_root / source.name
        shutil.copy2(source, candidate)
        with candidate.open(newline="", encoding="utf-8") as handle:
            rows = sum(1 for _ in csv.DictReader(handle))
        outputs[category] = {
            "file": f"candidates/{candidate.name}",
            "rows": rows,
            "sha256": hashlib.sha256(candidate.read_bytes()).hexdigest(),
        }
    report = normalization_root / "report.json"
    report.write_text(
        json.dumps(
            {
                "schema_version": NORMALIZATION_SCHEMA,
                "logical_date": DATE,
                "status": "ready",
                "prediction_eligible": True,
                "accepted_startlists_modified": False,
                "outputs": outputs,
            }
        ),
        encoding="utf-8",
    )
    return candidate_root, report


def write_workbook(path: Path, *, category: str, identity_drift: bool = False) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    workbook = Workbook()
    sheet = workbook.active
    sheet.title = str(CATEGORY_CONTRACTS[category]["sheet_prefix"])
    if category in {"men", "ladies"}:
        sheet.title += " 1"
        sheet.append(INDIVIDUAL_COLUMNS)
        sex = "M" if category == "men" else "L"
        sheet.append(("One", 1, "NOR", sex, 100, 50, 75, 90, 100, 100))
        second_id = 3 if identity_drift else 2
        sheet.append(("Two", second_id, "GER", sex, 100, 50, 75, 90, 100, 100))
    else:
        sheet.append(TEAM_COLUMNS)
        sex = "M" if category.startswith("men") else "L" if category.startswith("ladies") else "Mixed"
        race_type = (
            "Mixed Team Sprint" if category == "mixed_team_sprint" else
            "Mixed Team" if category == "mixed_team" else
            "Team Sprint" if category.endswith("team_sprint") else "Team"
        )
        sheet.append(("NOR", sex, race_type, 50, 75, 90, 100, 100))
        second_nation = "AUT" if identity_drift else "GER"
        sheet.append((second_nation, sex, race_type, 50, 75, 90, 100, 100))
    workbook.save(path)


def fake_conversion(command: list[str]) -> None:
    workbook_path = Path(command[2])
    output_root = Path(command[3])
    output_root.mkdir(parents=True, exist_ok=True)
    workbook = load_workbook(workbook_path, read_only=True, data_only=True)
    multiple = len(workbook.sheetnames) > 1
    for sheet in workbook.sheetnames:
        suffix = "" if not multiple else "_" + "".join(c if c.isalnum() else "_" for c in sheet)
        (output_root / f"{workbook_path.stem}{suffix}.json").write_text(
            json.dumps({"sheet_name": sheet}), encoding="utf-8"
        )
    workbook.close()


def runner(*, identity_drift: str | None = None):
    def fake_runner(command, **kwargs):
        if command[0] == "Rscript":
            environment = kwargs["env"]
            output = Path(environment["NORDIC_OUTPUT_ROOT"]) / DATE_KEY
            schedule = Path(environment["NORDIC_INPUT_ROOT"]) / "races.csv"
            with schedule.open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))
            for category, sex, race_type in CATEGORIES:
                if not any(row["Sex"] == sex and row["RaceType"] == race_type for row in rows):
                    continue
                write_workbook(
                    output / str(CATEGORY_CONTRACTS[category]["workbook"]),
                    category=category,
                    identity_drift=identity_drift == category,
                )
        else:
            fake_conversion(command)
        return subprocess.CompletedProcess(command, 0, stdout="")
    return fake_runner


class NordicCombinedPredictionShadowTests(unittest.TestCase):
    def test_normalized_official_candidates_replace_stale_repository_startlists(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root)
            candidate_root, normalization_report = build_normalized_startlists(root, ski)
            for category, _, _ in CATEGORIES:
                relative = next(
                    str(value)
                    for value in CATEGORY_CONTRACTS[category]["inputs"]
                    if "startlist" in Path(str(value)).name
                )
                accepted = ski / relative
                accepted.write_text(
                    accepted.read_text(encoding="utf-8").replace(DATE, "2026-02-09"),
                    encoding="utf-8",
                )
            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date=DATE,
                run_id="normalized-startlists",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                startlist_root=candidate_root,
                startlist_report=normalization_report,
                command_runner=runner(),
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))

        self.assertEqual("passed", report["status"])
        self.assertEqual("normalized_official_fis", report["startlist_source"])
        generated = [
            item for item in report["inputs"]
            if item.get("source") == "normalized_official_fis"
        ]
        self.assertEqual(8, len(generated))
        self.assertTrue(all(item["rows"] == 2 for item in generated))

    def test_normalized_candidate_checksum_drift_blocks_before_simulation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root, all_categories=False)
            candidate_root, normalization_report = build_normalized_startlists(
                root, ski, all_categories=False
            )
            candidate = candidate_root / "startlist_races_men.csv"
            candidate.write_text(candidate.read_text(encoding="utf-8") + "\n", encoding="utf-8")
            with self.assertRaisesRegex(PredictionShadowError, "checksum mismatch"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date=DATE,
                    run_id="checksum-drift",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    startlist_root=candidate_root,
                    startlist_report=normalization_report,
                    command_runner=lambda *args, **kwargs: self.fail("simulation unexpectedly ran"),
                )

    def test_normalized_categories_must_exactly_match_plan(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root, all_categories=False)
            candidate_root, normalization_report = build_normalized_startlists(
                root, ski, all_categories=False
            )
            document = json.loads(normalization_report.read_text(encoding="utf-8"))
            document["outputs"]["ladies"] = document["outputs"]["men"]
            normalization_report.write_text(json.dumps(document), encoding="utf-8")
            with self.assertRaisesRegex(PredictionShadowError, "categories do not match"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date=DATE,
                    run_id="category-drift",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    startlist_root=candidate_root,
                    startlist_report=normalization_report,
                    command_runner=lambda *args, **kwargs: self.fail("simulation unexpectedly ran"),
                )

    def test_all_eight_categories_are_isolated_validated_and_converted(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root)
            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date=DATE,
                run_id="all-categories",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                command_runner=runner(),
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))

        self.assertEqual("passed", report["status"])
        self.assertEqual("disabled", report["publication"]["status"])
        self.assertEqual(set(category for category, _, _ in CATEGORIES), set(report["startlist_rows"]))
        self.assertEqual(8, len(report["outputs"]["workbooks"]))
        self.assertEqual(8, len(report["outputs"]["json"]))
        self.assertTrue(all(item["materialized_as"].startswith("model-inputs/") for item in report["inputs"]))

    def test_prediction_identity_drift_fails_and_retains_report(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root, all_categories=False)
            with self.assertRaisesRegex(PredictionShadowError, "identities differ"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date=DATE,
                    run_id="identity-drift",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    command_runner=runner(identity_drift="men"),
                )
            report = json.loads(
                (root / "output/runs/identity-drift/reports/shadow-report.json").read_text()
            )
        self.assertEqual("failed", report["status"])
        self.assertEqual("disabled", report["publication"]["status"])

    def test_stale_dated_startlist_blocks_before_simulation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root, all_categories=False)
            startlist = ski / str(CATEGORY_CONTRACTS["men"]["inputs"][-1])
            text = startlist.read_text(encoding="utf-8").replace(DATE, "2026-02-09")
            startlist.write_text(text, encoding="utf-8")
            with self.assertRaisesRegex(PredictionShadowError, "does not exclusively match"):
                run_shadow(
                    blog_root=blog,
                    ski_root=ski,
                    output_root=root / "output",
                    logical_date=DATE,
                    run_id="stale-startlist",
                    n_simulations=25,
                    random_seed=42,
                    blog_commit="a" * 40,
                    ski_commit="b" * 40,
                    command_runner=lambda *args, **kwargs: self.fail("simulation unexpectedly ran"),
                )

    def test_no_race_is_a_successful_publication_disabled_no_op(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            blog, ski = build_runtime(root, all_categories=False)
            report_path = run_shadow(
                blog_root=blog,
                ski_root=ski,
                output_root=root / "output",
                logical_date="2026-02-11",
                run_id="no-race",
                n_simulations=25,
                random_seed=42,
                blog_commit="a" * 40,
                ski_commit="b" * 40,
                command_runner=lambda *args, **kwargs: self.fail("simulation unexpectedly ran"),
            )
            report = json.loads(report_path.read_text(encoding="utf-8"))
        self.assertEqual("skipped", report["status"])
        self.assertEqual("disabled", report["publication"]["status"])

    def test_manual_workflow_is_read_only_and_retains_evidence(self) -> None:
        source = WORKFLOW.read_text(encoding="utf-8")
        trigger = source.split("on:\n", 1)[1].split("\npermissions:", 1)[0]
        self.assertIn("workflow_dispatch:", trigger)
        self.assertNotIn("schedule:", trigger)
        self.assertNotIn("workflow_run:", trigger)
        self.assertNotIn("publish_predictions", source)
        self.assertIn("contents: read", source)
        self.assertIn("ref: ${{ steps.ski.outputs.sha }}", source)
        self.assertIn("-m pipelines.nordic_combined_prediction_shadow", source)
        self.assertIn("nordic_combined_startlist_acquire.py", source)
        self.assertIn("nordic_combined_startlist_normalize.py", source)
        self.assertIn("/pipelines/requirements-snapshot.txt", source)
        self.assertIn("Retain pre-run failure evidence", source)
        self.assertIn('"shadow runner did not produce its report"', source)
        self.assertIn(
            "STARTLIST_ACQUISITION_ROOT=$shadow_root/acquired-startlists", source
        )
        self.assertIn(
            "STARTLIST_NORMALIZATION_ROOT=$shadow_root/normalized-startlists", source
        )
        self.assertIn(
            "STARTLIST_ALIAS_FILE=$shadow_root/identity-aliases/fis_identity_aliases.csv",
            source,
        )
        self.assertNotIn("$shadow_root/startlists/", source)
        self.assertIn('--startlist-root "$STARTLIST_CANDIDATE_ROOT"', source)
        self.assertIn('--startlist-report "$STARTLIST_NORMALIZATION_REPORT"', source)
        self.assertNotIn("/excel365/startlist_races_men.csv", source)
        self.assertNotIn("/relay/excel365/startlist_team_races_men.csv", source)
        self.assertIn("continue-on-error: true", source)
        self.assertIn("if: always()", source)
        self.assertIn("retention-days: 30", source)
        self.assertNotIn("git push", source)
        self.assertNotIn("contents: write", source)


if __name__ == "__main__":
    unittest.main()
