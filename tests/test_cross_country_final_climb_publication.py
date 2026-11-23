import hashlib
import json
import shutil
import tempfile
import unittest
from datetime import date
from pathlib import Path
from unittest import mock

from openpyxl import Workbook

from pipelines.cross_country_final_climb_publication import (
    CANDIDATE_SCHEMA,
    RENDER_SCHEMA,
    FinalClimbPublicationError,
    build_final_climb_candidate,
    install_final_climb_candidate,
    render_final_climb_candidate,
)
from pipelines.cross_country_final_climb_shadow import (
    POINT_HEADERS,
    PROBABILITY_HEADERS,
    VALIDATION_SCHEMA,
)


def write_validated_outputs(
    root: Path, *, source_mode: str = "official_startlist"
) -> tuple[Path, Path]:
    output = root / "predictions" / "20270110"
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
            sheet.append((*identity, 50 - index * 5))
        workbook.save(output / f"{gender}.xlsx")

        workbook = Workbook()
        sheet = workbook.active
        sheet.title = "Men Race 1" if gender == "men" else "Ladies Race 1"
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
                "logical_date": "2027-01-10",
                "n_simulations": 100,
                "random_seed": 42,
                "participant_source_mode": source_mode,
                "publication": {"status": "disabled"},
                "sexes": {"M": {"rows": 2}, "L": {"rows": 2}},
                "outputs": outputs,
            }
        ),
        encoding="utf-8",
    )
    return root / "predictions", validation


class CrossCountryFinalClimbPublicationTests(unittest.TestCase):
    def test_builds_exact_publication_disabled_candidate(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            report = build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            self.assertEqual(CANDIDATE_SCHEMA, report["schema_version"])
            self.assertEqual("candidate", report["status"])
            self.assertEqual({"status": "disabled"}, report["publication"])
            self.assertEqual("official_startlist", report["participant_source_mode"])
            self.assertEqual(5, len(report["outputs"]))

            probability = json.loads(
                (
                    candidate
                    / "data/cross-country/drafts/final-climb-picks/20270110/men_position_probabilities.json"
                ).read_text()
            )
            self.assertEqual("Top 5", probability["headers"][5])
            self.assertEqual("Men One", probability["rows"][0][0])
            post = (
                candidate / "content/post/final-climb-picks/20270110.md"
            ).read_text()
            self.assertIn("Men's Points", post)
            self.assertIn("validated official Final Climb startlists", post)

    def test_historical_replay_candidate_is_unpublishable_and_labelled(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(
                root, source_mode="historical_results_replay"
            )
            candidate = root / "candidate"
            report = build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            self.assertEqual(
                "historical_results_replay", report["participant_source_mode"]
            )
            post = (
                candidate / "content/post/final-climb-picks/20270110.md"
            ).read_text()
            self.assertIn("cannot be published", post)
            self.assertIn('participant_source_mode: "historical_results_replay"', post)

    def test_rejects_changed_workbook_after_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            workbook = predictions / "20270110/men.xlsx"
            workbook.write_bytes(workbook.read_bytes() + b"tampered")
            with self.assertRaisesRegex(FinalClimbPublicationError, "checksum"):
                build_final_climb_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 10),
                    output_root=root / "candidate",
                )

    def test_rejects_enabled_or_unlabelled_validation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            document = json.loads(validation.read_text())
            document["publication"]["status"] = "enabled"
            validation.write_text(json.dumps(document))
            with self.assertRaisesRegex(FinalClimbPublicationError, "disabled"):
                build_final_climb_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 10),
                    output_root=root / "candidate",
                )
            document["publication"]["status"] = "disabled"
            document.pop("participant_source_mode")
            validation.write_text(json.dumps(document))
            with self.assertRaisesRegex(FinalClimbPublicationError, "source mode"):
                build_final_climb_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 10),
                    output_root=root / "candidate",
                )

    def test_refuses_unrecognized_nonempty_output_root(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            candidate.mkdir()
            (candidate / "unrelated.txt").write_text("keep", encoding="utf-8")
            with self.assertRaisesRegex(FinalClimbPublicationError, "existing"):
                build_final_climb_candidate(
                    prediction_root=predictions,
                    validation_report_path=validation,
                    logical_date=date(2027, 1, 10),
                    output_root=candidate,
                )

    def test_render_rejects_candidate_changed_after_build(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            men = (
                candidate
                / "data/cross-country/drafts/final-climb-picks/20270110/men.json"
            )
            men.write_text(men.read_text() + " ", encoding="utf-8")
            with (
                mock.patch(
                    "pipelines.cross_country_final_climb_publication.shutil.which",
                    return_value="/usr/bin/hugo",
                ),
                self.assertRaisesRegex(FinalClimbPublicationError, "checksum"),
            ):
                render_final_climb_candidate(
                    blog_root=Path(__file__).resolve().parents[1],
                    candidate_root=candidate,
                    logical_date=date(2027, 1, 10),
                    destination=root / "site",
                    report_path=root / "render.json",
                )

    @unittest.skipUnless(shutil.which("hugo"), "hugo is not installed")
    def test_renders_candidate_with_all_four_tables(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            report = render_final_climb_candidate(
                blog_root=Path(__file__).resolve().parents[1],
                candidate_root=candidate,
                logical_date=date(2027, 1, 10),
                destination=root / "site",
                report_path=root / "render.json",
            )
            self.assertEqual(RENDER_SCHEMA, report["schema_version"])
            self.assertEqual("passed", report["status"])
            self.assertEqual({"status": "disabled"}, report["publication"])
            self.assertEqual("official_startlist", report["participant_source_mode"])
            html = (
                root / "site/post/final-climb-picks/20270110/index.html"
            ).read_text()
            self.assertIn("Men One", html)
            self.assertIn("Women One", html)
            self.assertEqual(4, html.count('class="bbref-table"'))

            repository = root / "repository"
            repository.mkdir()
            (repository / ".git").write_text("gitdir: test\n", encoding="utf-8")
            installed = install_final_climb_candidate(
                candidate_root=candidate,
                render_report_path=root / "render.json",
                rendered_site_root=root / "site",
                logical_date=date(2027, 1, 10),
                as_of=date(2027, 1, 10),
                repository_root=repository,
            )
            self.assertEqual(5, len(installed))
            self.assertEqual(
                (candidate / "content/post/final-climb-picks/20270110.md").read_bytes(),
                (
                    repository / "content/post/final-climb-picks/20270110.md"
                ).read_bytes(),
            )

            render_report = root / "render.json"
            render_document = json.loads(render_report.read_text())
            render_document["participant_source_mode"] = "historical_results_replay"
            render_report.write_text(json.dumps(render_document), encoding="utf-8")
            with self.assertRaisesRegex(FinalClimbPublicationError, "not eligible"):
                install_final_climb_candidate(
                    candidate_root=candidate,
                    render_report_path=render_report,
                    rendered_site_root=root / "site",
                    logical_date=date(2027, 1, 10),
                    as_of=date(2027, 1, 10),
                    repository_root=repository,
                )

            render_document["participant_source_mode"] = "official_startlist"
            render_report.write_text(json.dumps(render_document), encoding="utf-8")
            page = root / "site/post/final-climb-picks/20270110/index.html"
            page.write_text(page.read_text() + "tampered", encoding="utf-8")
            with self.assertRaisesRegex(FinalClimbPublicationError, "checksum"):
                install_final_climb_candidate(
                    candidate_root=candidate,
                    render_report_path=render_report,
                    rendered_site_root=root / "site",
                    logical_date=date(2027, 1, 10),
                    as_of=date(2027, 1, 10),
                    repository_root=repository,
                )

    def test_install_rejects_historical_replay_and_extra_output(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(
                root, source_mode="historical_results_replay"
            )
            candidate = root / "candidate"
            build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            with self.assertRaisesRegex(FinalClimbPublicationError, "historical"):
                install_final_climb_candidate(
                    candidate_root=candidate,
                    render_report_path=root / "missing-render.json",
                    rendered_site_root=root / "missing-site",
                    logical_date=date(2027, 1, 10),
                    as_of=date(2027, 1, 10),
                    repository_root=root / "repository",
                )

            predictions, validation = write_validated_outputs(root / "official")
            candidate = root / "official-candidate"
            build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            extra = candidate / "unexpected.txt"
            extra.write_text("unexpected", encoding="utf-8")
            report_path = candidate / "publication_candidate_report.json"
            report = json.loads(report_path.read_text())
            report["outputs"]["unexpected.txt"] = {
                "sha256": hashlib.sha256(extra.read_bytes()).hexdigest()
            }
            report_path.write_text(json.dumps(report), encoding="utf-8")
            with self.assertRaisesRegex(FinalClimbPublicationError, "output set"):
                install_final_climb_candidate(
                    candidate_root=candidate,
                    render_report_path=root / "missing-render.json",
                    rendered_site_root=root / "missing-site",
                    logical_date=date(2027, 1, 10),
                    as_of=date(2027, 1, 10),
                    repository_root=root / "repository",
                )

    def test_install_rejects_past_prediction_before_writing(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            predictions, validation = write_validated_outputs(root)
            candidate = root / "candidate"
            build_final_climb_candidate(
                prediction_root=predictions,
                validation_report_path=validation,
                logical_date=date(2027, 1, 10),
                output_root=candidate,
            )
            with self.assertRaisesRegex(FinalClimbPublicationError, "past"):
                install_final_climb_candidate(
                    candidate_root=candidate,
                    render_report_path=root / "missing-render.json",
                    rendered_site_root=root / "missing-site",
                    logical_date=date(2027, 1, 10),
                    as_of=date(2027, 1, 11),
                    repository_root=root / "repository",
                )


if __name__ == "__main__":
    unittest.main()
