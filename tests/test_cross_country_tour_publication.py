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

from pipelines.cross_country_tour_publication import (
    CANDIDATE_SCHEMA,
    RENDER_SCHEMA,
    TourPublicationError,
    build_tour_candidate,
    install_tour_candidate,
    render_tour_candidate,
)
from pipelines.cross_country_tour_shadow import (
    POINT_HEADERS,
    PROBABILITY_HEADERS,
    VALIDATION_SCHEMA,
)


def write_validated_outputs(root: Path) -> tuple[Path, Path]:
    output = root / "predictions" / "20270101"
    output.mkdir(parents=True)
    identities = {
        "men": [("Men One", 1, "Norway"), ("Men Two", 2, "Sweden")],
        "ladies": [("Women One", 11, "Norway"), ("Women Two", 12, "Sweden")],
    }
    for gender, rows in identities.items():
        workbook = Workbook()
        sheet = workbook.active
        sheet.append(POINT_HEADERS)
        for index, identity in enumerate(rows):
            sheet.append((*identity, 200 - index * 20))
        workbook.save(output / f"{gender}.xlsx")

        workbook = Workbook()
        sheet = workbook.active
        sheet.title = "Men Tour de Ski" if gender == "men" else "Ladies Tour de Ski"
        sheet.append(PROBABILITY_HEADERS)
        for index, identity in enumerate(rows):
            sheet.append((*identity, 20 - index * 5, 40, 60, 80, 100))
        workbook.save(output / f"{gender}_position_probabilities.xlsx")

    outputs = {
        path.name: {"sha256": hashlib.sha256(path.read_bytes()).hexdigest()}
        for path in output.glob("*.xlsx")
    }
    validation = root / "validation.json"
    validation.write_text(
        json.dumps(
            {
                "schema_version": VALIDATION_SCHEMA,
                "status": "passed",
                "logical_date": "2027-01-01",
                "n_simulations": 100,
                "random_seed": 42,
                "publication": {"status": "disabled"},
                "sexes": {"M": {"rows": 2}, "L": {"rows": 2}},
                "outputs": outputs,
            }
        ),
        encoding="utf-8",
    )
    return root / "predictions", validation


@unittest.skipUnless(Workbook is not None, "openpyxl is not installed")
class CrossCountryTourPublicationTests(unittest.TestCase):
    def test_builds_exact_publication_disabled_candidate(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            report = build_tour_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 1),
                output_root=candidate,
            )
            self.assertEqual(CANDIDATE_SCHEMA, report["schema_version"])
            self.assertEqual("candidate", report["status"])
            self.assertEqual({"status": "disabled"}, report["publication"])
            self.assertEqual(5, len(report["outputs"]))

            probability = json.loads(
                (
                    candidate
                    / "data/cross-country/drafts/tds-picks/20270101/men_position_probabilities.json"
                ).read_text()
            )
            self.assertEqual("Top 5", probability["headers"][5])
            self.assertEqual("Men One", probability["rows"][0][0])
            post = (candidate / "content/post/tds-picks/20270101.md").read_text()
            self.assertIn("Men's Points", post)
            self.assertIn("Ladies' Position Probabilities", post)
            self.assertNotIn("Fantasy", post)
            self.assertNotIn("install", post.lower())

    def test_rejects_validation_with_enabled_publication(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            document = json.loads(validation.read_text())
            document["publication"]["status"] = "enabled"
            validation.write_text(json.dumps(document))
            with self.assertRaisesRegex(TourPublicationError, "disabled"):
                build_tour_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 1),
                    output_root=root / "candidate",
                )

    def test_rejects_workbook_changed_after_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            workbook = predictions / "20270101/men.xlsx"
            workbook.write_bytes(workbook.read_bytes() + b"tampered")
            with self.assertRaisesRegex(TourPublicationError, "checksum"):
                build_tour_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 1),
                    output_root=root / "candidate",
                )

    def test_rejects_mismatched_logical_date(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            with self.assertRaisesRegex(TourPublicationError, "logical date"):
                build_tour_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 2),
                    output_root=root / "candidate",
                )

    def test_refuses_unrecognized_nonempty_output_root(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            candidate.mkdir()
            (candidate / "unrelated.txt").write_text("keep", encoding="utf-8")
            with self.assertRaisesRegex(TourPublicationError, "existing"):
                build_tour_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 1),
                    output_root=candidate,
                )

    def test_render_rejects_candidate_changed_after_build(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_tour_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 1),
                output_root=candidate,
            )
            men = candidate / "data/cross-country/drafts/tds-picks/20270101/men.json"
            men.write_text(men.read_text() + " ", encoding="utf-8")
            with (
                mock.patch(
                    "pipelines.cross_country_tour_publication.shutil.which",
                    return_value="/usr/bin/hugo",
                ),
                self.assertRaisesRegex(TourPublicationError, "checksum"),
            ):
                render_tour_candidate(
                    blog_root=Path(__file__).resolve().parents[1],
                    candidate_root=candidate,
                    logical_date=date(2027, 1, 1),
                    destination=root / "site",
                    report_path=root / "render.json",
                )

    @unittest.skipUnless(shutil.which("hugo"), "hugo is not installed")
    def test_renders_candidate_with_all_four_tables(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_tour_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 1),
                output_root=candidate,
            )
            report = render_tour_candidate(
                blog_root=Path(__file__).resolve().parents[1],
                candidate_root=candidate,
                logical_date=date(2027, 1, 1),
                destination=root / "site",
                report_path=root / "render.json",
            )
            self.assertEqual(RENDER_SCHEMA, report["schema_version"])
            self.assertEqual("passed", report["status"])
            self.assertEqual({"status": "disabled"}, report["publication"])
            html = (root / "site/post/tds-picks/20270101/index.html").read_text()
            self.assertIn("Men One", html)
            self.assertIn("Women One", html)
            self.assertEqual(4, html.count('class="bbref-table"'))

            repository = root / "repository"
            repository.mkdir()
            (repository / ".git").write_text("gitdir: test\n", encoding="utf-8")
            installed = install_tour_candidate(
                candidate_root=candidate,
                render_report_path=root / "render.json",
                rendered_site_root=root / "site",
                logical_date=date(2027, 1, 1),
                as_of=date(2027, 1, 1),
                repository_root=repository,
            )
            self.assertEqual(5, len(installed))
            self.assertEqual(
                (candidate / "content/post/tds-picks/20270101.md").read_bytes(),
                (repository / "content/post/tds-picks/20270101.md").read_bytes(),
            )

    @unittest.skipUnless(shutil.which("hugo"), "hugo is not installed")
    def test_install_rejects_changed_render_and_past_prediction(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_tour_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 1),
                output_root=candidate,
            )
            render_tour_candidate(
                blog_root=Path(__file__).resolve().parents[1],
                candidate_root=candidate,
                logical_date=date(2027, 1, 1),
                destination=root / "site",
                report_path=root / "render.json",
            )
            repository = root / "repository"
            repository.mkdir()
            (repository / ".git").write_text("gitdir: test\n", encoding="utf-8")

            with self.assertRaisesRegex(TourPublicationError, "past"):
                install_tour_candidate(
                    candidate_root=candidate,
                    render_report_path=root / "render.json",
                    rendered_site_root=root / "site",
                    logical_date=date(2027, 1, 1),
                    as_of=date(2027, 1, 2),
                    repository_root=repository,
                )

            page = root / "site/post/tds-picks/20270101/index.html"
            page.write_text(page.read_text() + "tampered", encoding="utf-8")
            with self.assertRaisesRegex(TourPublicationError, "checksum"):
                install_tour_candidate(
                    candidate_root=candidate,
                    render_report_path=root / "render.json",
                    rendered_site_root=root / "site",
                    logical_date=date(2027, 1, 1),
                    as_of=date(2027, 1, 1),
                    repository_root=repository,
                )


if __name__ == "__main__":
    unittest.main()
