import csv
import hashlib
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_weekend_consumer import (
    BUNDLE_SCHEMA,
    SCHEDULE_FIELDS,
    STARTLIST_FIELDS,
    WeekendConsumerError,
    materialize_weekend_runtime,
)


def csv_bytes(fields: tuple[str, ...], rows: list[dict[str, str]]) -> bytes:
    import io

    output = io.StringIO(newline="")
    writer = csv.DictWriter(output, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return output.getvalue().encode()


def schedule_row(sex: str, race_id: str) -> dict[str, str]:
    return {
        "Date": "11/27/2026",
        "Race_Date": "11/28/2026",
        "Startlist": f"https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid={race_id}",
        "Sex": sex,
        "City": "Ruka",
        "Country": "Finland",
        "Distance": "10",
        "Technique": "C",
        "MS": "0",
        "Period": "1",
        "Pursuit": "0",
        "Stage": "0",
        "Final_Climb": "0",
        "Elevation": "337",
        "Race": "1",
        "Championship": "0",
    }


def startlist_row(
    sex: str,
    race_id: str,
    athlete_id: str,
    name: str,
    *,
    official: bool,
) -> dict[str, str]:
    return {
        "group_date": "2026-11-27",
        "race_date": "2026-11-28",
        "race_id": race_id,
        "sex": sex,
        "city": "Ruka",
        "country": "Finland",
        "distance": "10",
        "technique": "C",
        "canonical_athlete_id": athlete_id,
        "canonical_name": name,
        "canonical_nation": "Norway",
        "fis_competitor_id": f"9{athlete_id}" if official else "",
        "fis_code": f"3{athlete_id}" if official else "",
        "identity_match_method": "unique_normalized_name"
        if official
        else "accepted_history_id",
        "selection_status": "confirmed" if official else "candidate_pool",
        "source_mode": "official_fis_startlist"
        if official
        else "latest_season_fallback",
        "source_url": f"https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid={race_id}",
        "source_sha256": "a" * 64,
    }


def write_bundle(
    root: Path,
    *,
    schedule: list[dict[str, str]],
    men: list[dict[str, str]],
    ladies: list[dict[str, str]],
) -> Path:
    bundle = root / "bundle"
    bundle.mkdir()
    contents = {
        "schedule.csv": csv_bytes(SCHEDULE_FIELDS, schedule),
        "men_startlist.csv": csv_bytes(STARTLIST_FIELDS, men),
        "ladies_startlist.csv": csv_bytes(STARTLIST_FIELDS, ladies),
    }
    for name, content in contents.items():
        (bundle / name).write_bytes(content)
    report = {
        "schema_version": BUNDLE_SCHEMA,
        "status": "candidate",
        "logical_date": "2026-11-27",
        "publication": {"status": "disabled"},
        "outputs": {
            name: {
                "sha256": hashlib.sha256(content).hexdigest(),
                "rows": content.count(b"\n") - 1,
            }
            for name, content in contents.items()
        },
    }
    (bundle / "report.json").write_text(json.dumps(report), encoding="utf-8")
    return bundle


class CrossCountryWeekendConsumerTests(unittest.TestCase):
    def test_materializes_official_participation_matrix(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101"), schedule_row("M", "102")],
                men=[
                    startlist_row("M", "101", "1", "First Athlete", official=True),
                    startlist_row("M", "101", "2", "Second Athlete", official=True),
                    startlist_row("M", "102", "2", "Second Athlete", official=True),
                ],
                ladies=[],
            )
            report = materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
            )
            self.assertEqual(report["status"], "ready_for_shadow_simulation")
            self.assertEqual(report["publication"], {"status": "disabled"})
            with (root / "runtime/startlist_weekend_men.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual(
                [(row["ID"], row["Race1_Prob"], row["Race2_Prob"]) for row in rows],
                [("1", "1", "0"), ("2", "1", "1")],
            )
            self.assertEqual(rows[0]["Race1_Official"], "true")
            self.assertEqual(rows[0]["FIS_Code"], "31")
            self.assertTrue((root / "runtime/runtime_report.json").is_file())

    def test_fallback_probabilities_remain_unset_for_model(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("L", "201")],
                men=[],
                ladies=[
                    startlist_row("L", "201", "3", "Fallback Athlete", official=False)
                ],
            )
            materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
            )
            with (root / "runtime/startlist_weekend_ladies.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                row = next(csv.DictReader(handle))
            self.assertEqual(row["Race1_Prob"], "")
            self.assertEqual(row["Race1_Official"], "false")
            self.assertEqual(row["FIS_Code"], "")

    def test_preserves_known_fis_code_across_fallback_and_official_rows(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101"), schedule_row("M", "102")],
                men=[
                    startlist_row("M", "101", "1", "Athlete", official=False),
                    startlist_row("M", "102", "1", "Athlete", official=True),
                ],
                ladies=[],
            )
            materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
            )
            with (root / "runtime/startlist_weekend_men.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                row = next(csv.DictReader(handle))
            self.assertEqual("31", row["FIS_Code"])

    def test_rejects_tampered_bundle_output(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[startlist_row("M", "101", "1", "Athlete", official=True)],
                ladies=[],
            )
            (bundle / "men_startlist.csv").write_text("tampered\n", encoding="utf-8")
            with self.assertRaisesRegex(WeekendConsumerError, "checksum mismatch"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 27),
                    output_dir=root / "runtime",
                )

    def test_rejects_wrong_logical_date(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[startlist_row("M", "101", "1", "Athlete", official=True)],
                ladies=[],
            )
            with self.assertRaisesRegex(WeekendConsumerError, "logical date"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 28),
                    output_dir=root / "runtime",
                )

    def test_rejects_mixed_source_modes_for_one_event(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[
                    startlist_row("M", "101", "1", "Official", official=True),
                    startlist_row("M", "101", "2", "Fallback", official=False),
                ],
                ladies=[],
            )
            with self.assertRaisesRegex(WeekendConsumerError, "mixes"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 27),
                    output_dir=root / "runtime",
                )

    def test_does_not_replace_unrecognized_output_directory(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[startlist_row("M", "101", "1", "Athlete", official=True)],
                ladies=[],
            )
            output = root / "runtime"
            output.mkdir()
            protected = output / "keep.txt"
            protected.write_text("user data", encoding="utf-8")
            with self.assertRaisesRegex(WeekendConsumerError, "refusing to replace"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 27),
                    output_dir=output,
                )
            self.assertEqual(protected.read_text(encoding="utf-8"), "user data")


if __name__ == "__main__":
    unittest.main()
