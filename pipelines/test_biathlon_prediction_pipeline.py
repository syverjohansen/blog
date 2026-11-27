from __future__ import annotations

import csv
import hashlib
import json
import subprocess
from datetime import date
from pathlib import Path

import pytest
from openpyxl import Workbook

from pipelines.biathlon_prediction_publication import (
    BiathlonPublicationError,
    prepare_publication,
)
from pipelines.biathlon_prediction_shadow import (
    BiathlonShadowError,
    build_plan,
    run_shadow,
)


SHA = "a" * 40
DAY = "2026-11-27"


def _write_csv(path: Path, fields: list[str], rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def _fixture(tmp_path: Path) -> tuple[Path, Path, Path]:
    ski = tmp_path / "ski"
    accepted = ski / "elo/python/biathlon/polars/excel365"
    relay = ski / "elo/python/biathlon/polars/relay/excel365"
    _write_csv(
        accepted / "races.csv",
        ["Date", "Startlist", "Sex", "City", "Country", "Distance", "RaceType", "Race"],
        [{"Date": "11/27/2026", "Startlist": "official", "Sex": "L", "City": "Kontiolahti", "Country": "Finland", "Distance": 15, "RaceType": "Individual", "Race": 1}],
    )
    history_fields = ["Date", "ID", "Skier", "Nation", "Race", "RaceType", "Place"]
    for name in ("men_chrono.csv", "ladies_chrono.csv"):
        _write_csv(accepted / name, history_fields, [{"Date": "2026-03-01", "ID": 1, "Skier": "History", "Nation": "NOR", "Race": 1, "RaceType": "Sprint", "Place": 1}])
    for name in ("men_relay_chrono.csv", "ladies_relay_chrono.csv", "mixed_relay_chrono.csv", "single_mixed_relay_chrono.csv"):
        _write_csv(relay / name, ["Nation", "Date"], [{"Nation": "NOR", "Date": "2026-03-01"}])

    normalized = tmp_path / "normalized"
    candidate = normalized / "candidates/startlist_races_ladies.csv"
    _write_csv(
        candidate,
        ["ID", "Skier", "Nation", "Sex", "Race1_Prob", "Race_Date"],
        [
            {"ID": 101, "Skier": "Athlete One", "Nation": "NOR", "Sex": "L", "Race1_Prob": 1, "Race_Date": "11/27/2026"},
            {"ID": 102, "Skier": "Athlete Two", "Nation": "SWE", "Sex": "L", "Race1_Prob": 1, "Race_Date": "11/27/2026"},
        ],
    )
    digest = hashlib.sha256(candidate.read_bytes()).hexdigest()
    report = normalized / "report.json"
    report.write_text(json.dumps({
        "schema_version": "nordicnumbers.biathlon-startlist-normalization/v1",
        "status": "ready", "prediction_eligible": True, "logical_date": DAY,
        "accepted_startlists_modified": False,
        "outputs": {"ladies": {"file": "candidates/startlist_races_ladies.csv", "rows": 2, "sha256": digest}},
    }), encoding="utf-8")
    return ski, candidate.parent, report


def _runner(command, *, env, **kwargs):
    output = Path(env["NORDIC_OUTPUT_ROOT"]) / "20261127"
    output.mkdir(parents=True)
    workbook = Workbook()
    sheet = workbook.active
    sheet.title = "Ladies Race 1"
    sheet.append(["Skier", "ID", "Nation", "Sex", "Participation", "Win", "Podium", "Top-5", "Top-10", "Top-30"])
    sheet.append(["Athlete One", 101, "NOR", "L", 100, 60, 80, 90, 100, 100])
    sheet.append(["Athlete Two", 102, "SWE", "L", 100, 40, 70, 90, 100, 100])
    workbook.save(output / "ladies_position_probabilities.xlsx")
    return subprocess.CompletedProcess(command, 0)


def test_plan_selects_exact_date_and_category(tmp_path: Path) -> None:
    ski, _, _ = _fixture(tmp_path)
    plan = build_plan(ski / "elo/python/biathlon/polars/excel365/races.csv", date.fromisoformat(DAY))
    assert plan["status"] == "ready"
    assert plan["categories"]["ladies"] == 1
    assert sum(plan["categories"].values()) == 1


def test_shadow_and_publication_are_guarded_end_to_end(tmp_path: Path) -> None:
    ski, candidates, normalization = _fixture(tmp_path)
    blog = Path(__file__).resolve().parents[1]
    shadow_root = tmp_path / "shadow"
    shadow_report = run_shadow(
        blog_root=blog, ski_root=ski, output_root=shadow_root,
        logical_date=DAY, run_id="test-run", n_simulations=25, random_seed=7,
        blog_commit=SHA, ski_commit=SHA, startlist_root=candidates,
        startlist_report=normalization, command_runner=_runner,
    )
    shadow = json.loads(shadow_report.read_text())
    assert shadow["status"] == "passed"
    assert shadow["publication"]["status"] == "disabled"
    assert shadow["startlist_source"] == "normalized_official_ibu"

    repository = tmp_path / "publication"
    report = tmp_path / "publication-report.json"
    prepared = prepare_publication(
        shadow_report=shadow_report, normalization_report=normalization,
        workbook_root=shadow_root / "runs/test-run/workbooks/20261127",
        repository_root=repository, report_path=report,
        evaluation_date=date(2026, 11, 26),
    )
    assert prepared["status"] == "prepared"
    post = repository / "race-picks/fragments/20261127/biathlon.md"
    assert "draft: false" in post.read_text()
    assert "biathlon/datatable" in post.read_text()
    assert (repository / "data/biathlon/drafts/race-picks/20261127/ladies_position_probabilities.json").is_file()


def test_shadow_fails_closed_on_roster_mismatch(tmp_path: Path) -> None:
    ski, candidates, normalization = _fixture(tmp_path)

    def wrong_runner(command, *, env, **kwargs):
        result = _runner(command, env=env, **kwargs)
        workbook_path = Path(env["NORDIC_OUTPUT_ROOT"]) / "20261127/ladies_position_probabilities.xlsx"
        workbook = load_workbook(workbook_path)
        workbook.active.delete_rows(3)
        workbook.save(workbook_path)
        return result

    from openpyxl import load_workbook

    with pytest.raises(BiathlonShadowError, match="roster mismatch"):
        run_shadow(
            blog_root=Path(__file__).resolve().parents[1], ski_root=ski,
            output_root=tmp_path / "shadow", logical_date=DAY, run_id="bad-run",
            n_simulations=10, random_seed=1, blog_commit=SHA, ski_commit=SHA,
            startlist_root=candidates, startlist_report=normalization,
            command_runner=wrong_runner,
        )


def test_publication_rejects_nonpassed_shadow(tmp_path: Path) -> None:
    shadow = tmp_path / "runs/test/reports/shadow-report.json"
    shadow.parent.mkdir(parents=True)
    shadow.write_text(json.dumps({
        "schema_version": "nordicnumbers.biathlon-prediction-shadow/v1",
        "status": "failed", "publication": {"status": "disabled"},
    }))
    with pytest.raises(BiathlonPublicationError, match="passed official-IBU"):
        prepare_publication(
            shadow_report=shadow, normalization_report=tmp_path / "missing.json",
            workbook_root=tmp_path, repository_root=tmp_path,
            report_path=tmp_path / "report.json",
        )


def test_r_runtime_uses_explicit_hosted_contract() -> None:
    script = (Path(__file__).resolve().parents[1] / "content/post/biathlon/drafts/race-picks-simulation.R").read_text()
    for variable in (
        "NORDIC_INPUT_ROOT", "NORDIC_RELAY_INPUT_ROOT", "NORDIC_OUTPUT_ROOT",
        "NORDIC_LOGICAL_DATE", "NORDIC_N_SIMULATIONS", "NORDIC_RANDOM_SEED",
    ):
        assert variable in script
    assert 'current_date <- as.Date(format(Sys.time(), tz = "UTC"))' not in script


def test_workflow_retries_and_publishes_without_manual_approval() -> None:
    root = Path(__file__).resolve().parents[1]
    workflow = (root / ".github/workflows/biathlon-predictions.yml").read_text()
    assert 'cron: "23 10,14,18,22 * 1-3,11-12 *"' in workflow
    assert "biathlon_startlist_acquire.py" in workflow
    assert "biathlon_startlist_normalize.py" in workflow
    assert "pipelines.biathlon_prediction_shadow" in workflow
    assert "pipelines.biathlon_prediction_publication" in workflow
    assert "publish_predictions" not in workflow
    assert "contents: read" in workflow
    assert "contents: write" in workflow
    assert "needs: shadow" in workflow
    assert "commits/main" in workflow
    contracts = json.loads((root / "pipelines/sport_publication_contracts.json").read_text())
    assert contracts["biathlon"]["automation_status"] == "active"
