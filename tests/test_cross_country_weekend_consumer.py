import csv
import hashlib
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_weekend_consumer import (
    BUNDLE_SCHEMA,
    ELO_STREAMS,
    QUOTA_RUNTIME_FIELDS,
    QUOTA_SOURCE_FIELDS,
    SCHEDULE_FIELDS,
    SHADOW_RATING_FIELDS,
    SHADOW_RATING_QUOTA_FIELDS,
    SHADOW_RATING_SCHEMA,
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
        "identity_match_method": (
            "unique_normalized_name" if official else "accepted_history_id"
        ),
        "selection_status": "confirmed" if official else "candidate_pool",
        "source_mode": (
            "official_fis_startlist" if official else "latest_season_fallback"
        ),
        "source_url": f"https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid={race_id}",
        "source_sha256": "a" * 64,
    }


def with_quota(
    row: dict[str, str],
    *,
    base: int,
    shared: int,
    personal_right: str = "",
    host: bool = False,
    team_event: bool = False,
) -> dict[str, str]:
    return {
        **row,
        "quota_contract_version": "fis-cc-world-cup-2026-27-period-1-v1",
        "base_nation_quota": str(base),
        "shared_event_quota": str(shared),
        "personal_quota": "1" if personal_right else "0",
        "personal_start_right": personal_right,
        "is_host_nation": "1" if host else "0",
        "team_event_weekend_exception": "1" if team_event else "0",
        "max_nation_starters": "10",
    }


def write_bundle(
    root: Path,
    *,
    schedule: list[dict[str, str]],
    men: list[dict[str, str]],
    ladies: list[dict[str, str]],
    quota_contract: bool = False,
    event_modes: dict[tuple[str, str], str] | None = None,
) -> Path:
    bundle = root / "bundle"
    bundle.mkdir()
    contents = {
        "schedule.csv": csv_bytes(SCHEDULE_FIELDS, schedule),
        "men_startlist.csv": csv_bytes(
            (*STARTLIST_FIELDS, *QUOTA_SOURCE_FIELDS)
            if quota_contract
            else STARTLIST_FIELDS,
            men,
        ),
        "ladies_startlist.csv": csv_bytes(
            (*STARTLIST_FIELDS, *QUOTA_SOURCE_FIELDS)
            if quota_contract
            else STARTLIST_FIELDS,
            ladies,
        ),
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
        "events": [
            {
                "sex": row["Sex"],
                "race_id": row["Startlist"].rsplit("=", 1)[-1],
                "source_mode": (event_modes or {}).get(
                    (row["Sex"], row["Startlist"].rsplit("=", 1)[-1]),
                    "official_fis_startlist"
                    if any(
                        item["race_id"] == row["Startlist"].rsplit("=", 1)[-1]
                        and item["source_mode"] == "official_fis_startlist"
                        for item in (*men, *ladies)
                    )
                    else "latest_season_fallback",
                ),
            }
            for row in schedule
        ],
    }
    if quota_contract:
        report["quota_contract"] = {
            "version": "fis-cc-world-cup-2026-27-period-1-v1",
            "season": 2027,
            "sha256": "b" * 64,
        }
    (bundle / "report.json").write_text(json.dumps(report), encoding="utf-8")
    return bundle


def write_shadow_ratings(
    root: Path,
    *,
    gender: str,
    rows: list[dict[str, str]],
) -> None:
    directory = root / gender
    directory.mkdir(parents=True)
    fields = (
        SHADOW_RATING_QUOTA_FIELDS
        if rows and all(field in rows[0] for field in QUOTA_SOURCE_FIELDS)
        else SHADOW_RATING_FIELDS
    )
    payload = csv_bytes(fields, rows)
    (directory / "startlist_prediction_elo_shadow.csv").write_bytes(payload)
    report = {
        "schema_version": SHADOW_RATING_SCHEMA,
        "status": "shadow_candidate",
        "publication": {"status": "disabled"},
        "selected_k_factor": 4.0,
        "rows": len(rows),
        "output_sha256": hashlib.sha256(payload).hexdigest(),
    }
    (directory / "report.json").write_text(json.dumps(report), encoding="utf-8")


def rated_row(row: dict[str, str], source: str, value: str) -> dict[str, str]:
    return {
        **row,
        "rating_source": source,
        "rating_as_of_date": "2026-11-01" if source != "newcomer_prior" else "",
        "rating_k_factor": "" if source == "accepted_official_elo" else "4.0",
        **{stream: value for stream in ELO_STREAMS},
    }


class CrossCountryWeekendConsumerTests(unittest.TestCase):
    def test_materializes_audited_quota_contract_for_r_model(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            ordinary = with_quota(
                startlist_row("M", "101", "1", "Ordinary", official=False),
                base=6,
                shared=6,
            )
            personal = with_quota(
                startlist_row("M", "101", "2", "Personal", official=False),
                base=6,
                shared=6,
                personal_right="continental_cup",
            )
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[ordinary, personal],
                ladies=[],
                quota_contract=True,
            )
            ratings = root / "ratings"
            write_shadow_ratings(
                ratings,
                gender="men",
                rows=[
                    rated_row(ordinary, "accepted_official_elo", "1650"),
                    rated_row(personal, "accepted_official_elo", "1625"),
                ],
            )
            report = materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
                prediction_elo_root=ratings,
            )
            with (root / "runtime/startlist_weekend_men.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = list(csv.DictReader(handle))
            self.assertTrue(set(QUOTA_RUNTIME_FIELDS) <= set(rows[0]))
            self.assertEqual(rows[0]["Shared_Event_Quota"], "6")
            self.assertEqual(rows[1]["Personal_Quota"], "1")
            self.assertEqual(rows[1]["Personal_Start_Right"], "continental_cup")
            self.assertEqual(
                report["quota_contract"],
                {
                    "enabled": True,
                    "version": "fis-cc-world-cup-2026-27-period-1-v1",
                },
            )

    def test_rejects_transferable_or_malformed_personal_quota(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            malformed = with_quota(
                startlist_row("L", "201", "3", "Athlete", official=False),
                base=5,
                shared=5,
            )
            malformed["personal_quota"] = "1"
            bundle = write_bundle(
                root,
                schedule=[schedule_row("L", "201")],
                men=[],
                ladies=[malformed],
                quota_contract=True,
            )
            with self.assertRaisesRegex(WeekendConsumerError, "quota assignment"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 27),
                    output_dir=root / "runtime",
                )

    def test_materializes_validated_private_ratings_for_shadow_model(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            known = startlist_row("M", "101", "1", "Known Athlete", official=True)
            unknown = startlist_row(
                "M", "101", "fis:999", "Unknown Athlete", official=True
            )
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[known, unknown],
                ladies=[],
            )
            ratings = root / "ratings"
            write_shadow_ratings(
                ratings,
                gender="men",
                rows=[
                    rated_row(known, "accepted_official_elo", "1650"),
                    rated_row(unknown, "fixed_anchor_prediction_elo", "1425"),
                ],
            )
            report = materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
                prediction_elo_root=ratings,
            )
            with (root / "runtime/startlist_weekend_men.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = {row["ID"]: row for row in csv.DictReader(handle)}
            self.assertEqual(rows["1"]["Shadow_Rating_Source"], "accepted_official_elo")
            self.assertEqual(
                rows["fis:999"]["Shadow_Rating_Source"],
                "fixed_anchor_prediction_elo",
            )
            self.assertEqual(rows["fis:999"]["Elo"], "1425")
            self.assertEqual(rows["fis:999"]["Shadow_Rating_K_Factor"], "4.0")
            self.assertEqual(
                report["prediction_elo_shadow"],
                {"enabled": True, "selected_k_factor": 4.0},
            )
            self.assertEqual(report["publication"], {"status": "disabled"})

    def test_rejects_shadow_rating_identity_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            unknown = startlist_row(
                "M", "101", "fis:999", "Unknown Athlete", official=True
            )
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101")],
                men=[unknown],
                ladies=[],
            )
            drifted = rated_row(unknown, "fixed_anchor_prediction_elo", "1425")
            drifted["fis_code"] = "changed"
            ratings = root / "ratings"
            write_shadow_ratings(ratings, gender="men", rows=[drifted])
            with self.assertRaisesRegex(WeekendConsumerError, "identity differs"):
                materialize_weekend_runtime(
                    bundle_dir=bundle,
                    logical_date=date(2026, 11, 27),
                    output_dir=root / "runtime",
                    prediction_elo_root=ratings,
                )

    def test_local_identity_accepts_private_rating_with_explicit_fis_identity(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            athlete = startlist_row(
                "L", "201", "6320", "Ida Lien", official=True
            )
            athlete["selection_status"] = "announced"
            athlete["source_mode"] = "announced_roster_evidence"
            bundle = write_bundle(
                root,
                schedule=[schedule_row("L", "201")],
                men=[],
                ladies=[athlete],
                event_modes={("L", "201"): "announced_roster_overlay"},
            )
            ratings = root / "ratings"
            write_shadow_ratings(
                ratings,
                gender="ladies",
                rows=[rated_row(athlete, "fixed_anchor_prediction_elo", "1425")],
            )
            materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
                prediction_elo_root=ratings,
            )
            with (root / "runtime/startlist_weekend_ladies.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                selected = next(csv.DictReader(handle))
            self.assertEqual("6320", selected["ID"])
            self.assertEqual("fixed_anchor_prediction_elo", selected["Shadow_Rating_Source"])
            self.assertEqual("1425", selected["Elo"])

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

    def test_announced_overlay_preserves_exact_and_historical_race_semantics(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            exact = startlist_row("M", "101", "1", "Exact Athlete", official=False)
            exact.update(
                selection_status="announced",
                source_mode="announced_roster_evidence",
            )
            weekend = startlist_row(
                "M", "101", "3", "Weekend Athlete", official=False
            )
            weekend.update(source_mode="announced_weekend_pool")
            weekend_second = {**weekend, "race_id": "102"}
            fallback_second = startlist_row(
                "M", "102", "1", "Exact Athlete", official=False
            )
            excluded_elsewhere = startlist_row(
                "M", "102", "2", "Excluded Athlete", official=False
            )
            bundle = write_bundle(
                root,
                schedule=[schedule_row("M", "101"), schedule_row("M", "102")],
                men=[exact, weekend, fallback_second, excluded_elsewhere, weekend_second],
                ladies=[],
                event_modes={
                    ("M", "101"): "announced_roster_overlay",
                    ("M", "102"): "announced_roster_overlay",
                },
            )

            materialize_weekend_runtime(
                bundle_dir=bundle,
                logical_date=date(2026, 11, 27),
                output_dir=root / "runtime",
            )
            with (root / "runtime/startlist_weekend_men.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                rows = {row["ID"]: row for row in csv.DictReader(handle)}

            self.assertEqual(
                ("1", "0"),
                (rows["1"]["Race1_Prob"], rows["1"]["Race2_Prob"]),
            )
            self.assertEqual(
                ("0", ""),
                (rows["2"]["Race1_Prob"], rows["2"]["Race2_Prob"]),
            )
            self.assertEqual(
                ("", ""),
                (rows["3"]["Race1_Prob"], rows["3"]["Race2_Prob"]),
            )
            runtime_report = json.loads(
                (root / "runtime/runtime_report.json").read_text()
            )
            self.assertEqual(
                "announced_roster_overlay",
                runtime_report["event_modes"]["M/101"],
            )

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
