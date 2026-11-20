import hashlib
import json
import shutil
import tempfile
import unittest
from datetime import date
from pathlib import Path
from unittest import mock

try:
    from openpyxl import Workbook
except ModuleNotFoundError:
    Workbook = None

from pipelines.cross_country_weekend_publication import (
    CANDIDATE_SCHEMA,
    RENDER_SCHEMA,
    WeekendPublicationError,
    build_weekend_candidate,
    install_weekend_candidate,
    render_weekend_candidate,
    resolve_active_weekend_group_date,
)
from pipelines.cross_country_weekend_shadow import (
    EXPECTED_HEADERS,
    VALIDATION_SCHEMA,
)


def write_evidence(root: Path, *, fallback: bool = True) -> tuple[Path, Path, Path]:
    workbook_path = root / "weekend.xlsx"
    workbook = Workbook()
    men = workbook.active
    men.title = "Men Top 30"
    men.append(EXPECTED_HEADERS)
    men.append(("First Man", 101, "Norway", "M", 150.5, 3, 20.0, 50.0))
    ladies = workbook.create_sheet("Ladies Top 30")
    ladies.append(EXPECTED_HEADERS)
    ladies.append(("First Woman", 201, "Sweden", "L", 160.5, 3, 25.0, 55.0))
    workbook.save(workbook_path)
    workbook_sha = hashlib.sha256(workbook_path.read_bytes()).hexdigest()

    runtime_path = root / "runtime_report.json"
    runtime_path.write_text(
        json.dumps(
            {
                "schema_version": "nordicnumbers.cross-country-weekend-runtime/v1",
                "logical_date": "2026-11-27",
                "publication": {"status": "disabled"},
                "event_modes": {
                    "M/101": "latest_season_fallback"
                    if fallback
                    else "official_fis_startlist",
                    "L/102": "official_fis_startlist",
                },
            }
        ),
        encoding="utf-8",
    )
    validation_path = root / "validation_report.json"
    validation_path.write_text(
        json.dumps(
            {
                "schema_version": VALIDATION_SCHEMA,
                "status": "passed",
                "logical_date": "2026-11-27",
                "publication": {"status": "disabled"},
                "sheets": {
                    "Men Top 30": {"rows": 1, "sex": "M"},
                    "Ladies Top 30": {"rows": 1, "sex": "L"},
                },
                "outputs": {
                    "weekend_top_30_predicted_points.xlsx": {"sha256": workbook_sha}
                },
            }
        ),
        encoding="utf-8",
    )
    return workbook_path, runtime_path, validation_path


def write_schedule(root: Path) -> Path:
    schedule = root / "weekends.csv"
    schedule.write_text(
        "Date,Race_Date,Distance,Stage\n"
        "11/27/2026,11/27/2026,Sprint,0\n"
        "11/27/2026,11/28/2026,10,0\n"
        "11/27/2026,11/29/2026,Rel,0\n"
        "12/29/2026,01/01/2027,10,1\n",
        encoding="utf-8",
    )
    return schedule


@unittest.skipUnless(Workbook is not None, "openpyxl is not installed")
class CrossCountryWeekendPublicationTests(unittest.TestCase):
    def test_resolves_active_weekend_to_stable_group_date(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = write_schedule(Path(directory))
            self.assertEqual(
                resolve_active_weekend_group_date(
                    schedule_path=schedule, as_of=date(2026, 11, 28)
                ),
                date(2026, 11, 27),
            )
            self.assertEqual(
                resolve_active_weekend_group_date(
                    schedule_path=schedule, as_of=date(2026, 11, 30)
                ),
                date(2026, 11, 30),
            )

    def test_builds_stable_json_and_candidate_post(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root)
            output = root / "candidate"
            report = build_weekend_candidate(
                workbook_path=workbook,
                runtime_report_path=runtime,
                validation_report_path=validation,
                logical_date=date(2026, 11, 27),
                output_root=output,
            )
            self.assertEqual(report["schema_version"], CANDIDATE_SCHEMA)
            self.assertEqual(report["status"], "candidate")
            self.assertEqual(report["source_state"], "provisional")
            self.assertEqual(report["publication"], {"status": "disabled"})
            self.assertEqual(report["row_count"], 2)

            men = json.loads(
                (
                    output / "data/cross-country/weekly-picks/20261127/men.json"
                ).read_text()
            )
            self.assertEqual(men["headers"], list(EXPECTED_HEADERS))
            self.assertEqual(men["rows"][0][1], 101)
            post = (output / "content/post/weekly-picks/20261127.md").read_text()
            self.assertIn("validated latest-season fallback pools", post)
            self.assertIn('weekend_source_state: "provisional"', post)
            self.assertIn(
                'cross-country/datatable "cross-country/weekly-picks/20261127/men"',
                post,
            )
            self.assertNotIn("Fantasy", post)

    def test_official_candidate_discloses_official_startlists(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root, fallback=False)
            report = build_weekend_candidate(
                workbook_path=workbook,
                runtime_report_path=runtime,
                validation_report_path=validation,
                logical_date=date(2026, 11, 27),
                output_root=root / "candidate",
            )
            self.assertEqual(report["source_state"], "official")
            post = (
                root / "candidate/content/post/weekly-picks/20261127.md"
            ).read_text()
            self.assertIn("official FIS startlists", post)

    def test_rejects_workbook_changed_after_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root)
            workbook.write_bytes(workbook.read_bytes() + b"tampered")
            with self.assertRaisesRegex(WeekendPublicationError, "checksum"):
                build_weekend_candidate(
                    workbook_path=workbook,
                    runtime_report_path=runtime,
                    validation_report_path=validation,
                    logical_date=date(2026, 11, 27),
                    output_root=root / "candidate",
                )

    def test_refuses_unrecognized_nonempty_output_root(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root)
            output = root / "candidate"
            output.mkdir()
            (output / "unrelated.txt").write_text("keep", encoding="utf-8")
            with self.assertRaisesRegex(WeekendPublicationError, "existing"):
                build_weekend_candidate(
                    workbook_path=workbook,
                    runtime_report_path=runtime,
                    validation_report_path=validation,
                    logical_date=date(2026, 11, 27),
                    output_root=output,
                )

    @unittest.skipUnless(shutil.which("hugo"), "hugo is not installed")
    def test_renders_candidate_with_expected_tables_and_athletes(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root)
            candidate = root / "candidate"
            build_weekend_candidate(
                workbook_path=workbook,
                runtime_report_path=runtime,
                validation_report_path=validation,
                logical_date=date(2026, 11, 27),
                output_root=candidate,
            )
            report_path = root / "reports/render_report.json"
            report = render_weekend_candidate(
                blog_root=Path(__file__).resolve().parents[1],
                candidate_root=candidate,
                logical_date=date(2026, 11, 27),
                destination=root / "site",
                report_path=report_path,
            )
            self.assertEqual(report["schema_version"], RENDER_SCHEMA)
            self.assertEqual(report["status"], "passed")
            self.assertEqual(report["publication"], {"status": "disabled"})
            html = (root / "site/post/weekly-picks/20261127/index.html").read_text()
            self.assertIn("First Man", html)
            self.assertIn("First Woman", html)
            self.assertIn("table-cross-countryweekly-picks20261127men", html)
            self.assertTrue(report_path.is_file())
            repository = root / "repository"
            repository.mkdir()
            (repository / ".git").write_text("gitdir: test\n", encoding="utf-8")
            installed = install_weekend_candidate(
                candidate_root=candidate,
                render_report_path=report_path,
                rendered_site_root=root / "site",
                logical_date=date(2026, 11, 27),
                repository_root=repository,
            )
            self.assertEqual(len(installed), 3)
            self.assertEqual(
                (repository / "content/post/weekly-picks/20261127.md").read_bytes(),
                (candidate / "content/post/weekly-picks/20261127.md").read_bytes(),
            )
            (repository / "content/post/weekly-picks/20261127.md").write_text(
                'weekend_source_state: "official"\n', encoding="utf-8"
            )
            with self.assertRaisesRegex(WeekendPublicationError, "official"):
                install_weekend_candidate(
                    candidate_root=candidate,
                    render_report_path=report_path,
                    rendered_site_root=root / "site",
                    logical_date=date(2026, 11, 27),
                    repository_root=repository,
                )

    def test_render_rejects_candidate_changed_after_build(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            workbook, runtime, validation = write_evidence(root)
            candidate = root / "candidate"
            build_weekend_candidate(
                workbook_path=workbook,
                runtime_report_path=runtime,
                validation_report_path=validation,
                logical_date=date(2026, 11, 27),
                output_root=candidate,
            )
            men = candidate / "data/cross-country/weekly-picks/20261127/men.json"
            men.write_text(men.read_text() + " ", encoding="utf-8")
            with (
                mock.patch(
                    "pipelines.cross_country_weekend_publication.shutil.which",
                    return_value="/usr/bin/hugo",
                ),
                self.assertRaisesRegex(WeekendPublicationError, "checksum"),
            ):
                render_weekend_candidate(
                    blog_root=Path(__file__).resolve().parents[1],
                    candidate_root=candidate,
                    logical_date=date(2026, 11, 27),
                    destination=root / "site",
                    report_path=root / "render.json",
                )


if __name__ == "__main__":
    unittest.main()
