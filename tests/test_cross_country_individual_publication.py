import csv
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_individual_publication import (
    IndividualEvent,
    IndividualPublicationError,
    detect_individual_events,
    prepare_runtime,
    render_post,
    write_post,
)

SCHEDULE_FIELDS = ("Date", "Distance", "Sex", "Technique", "City")


def _write_csv(path: Path, fieldnames: tuple[str, ...], rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _write_prediction(path: Path, *, invalid: bool = False) -> None:
    document = {
        "sheet_name": "Men Race 1",
        "headers": [
            "Skier",
            "ID",
            "Nation",
            "Win",
            "Podium",
            "Top-5",
            "Top-10",
            "Top-30",
        ],
        "rows": [
            ["Example Skier", 123, "Norway", 101 if invalid else 20, 50, 70, 90, 100]
        ],
    }
    path.write_text(json.dumps(document), encoding="utf-8")


class CrossCountryIndividualPublicationTests(unittest.TestCase):
    def test_detects_individual_events_and_excludes_team_events(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            _write_csv(
                schedule,
                SCHEDULE_FIELDS,
                [
                    {
                        "Date": "11/27/2026",
                        "Distance": "Sprint",
                        "Sex": "M",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                    {
                        "Date": "11/27/2026",
                        "Distance": "Sprint",
                        "Sex": "L",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                    {
                        "Date": "11/27/2026",
                        "Distance": "Ts",
                        "Sex": "M",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                    {
                        "Date": "11/28/2026",
                        "Distance": "10",
                        "Sex": "M",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                ],
            )
            self.assertEqual(
                detect_individual_events(schedule, date(2026, 11, 27)),
                (
                    IndividualEvent(gender="ladies", distance="Sprint", technique="C"),
                    IndividualEvent(gender="men", distance="Sprint", technique="C"),
                ),
            )

    def test_prepares_isolated_schedule_and_current_elo_inputs(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            data = root / "data"
            schedule = data / "races.csv"
            _write_csv(
                schedule,
                SCHEDULE_FIELDS,
                [
                    {
                        "Date": "11/27/2026",
                        "Distance": "Sprint",
                        "Sex": "M",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                    {
                        "Date": "11/27/2026",
                        "Distance": "Rel",
                        "Sex": "M",
                        "Technique": "N/A",
                        "City": "Ruka",
                    },
                    {
                        "Date": "11/28/2026",
                        "Distance": "10",
                        "Sex": "L",
                        "Technique": "C",
                        "City": "Ruka",
                    },
                ],
            )
            history_fields = ("Date", "City", "ID", "Skier")
            history_rows = [
                {"Date": "2026-03-22", "City": "Ruka", "ID": "1", "Skier": "A"}
            ]
            for gender in ("men", "ladies"):
                _write_csv(data / f"{gender}_chrono.csv", history_fields, history_rows)
            _write_csv(
                data / "elevation.csv",
                ("City", "Elevation"),
                [{"City": "Ruka", "Elevation": "200"}],
            )

            prepare_runtime(schedule, date(2026, 11, 27), data)

            with schedule.open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual(len(rows), 1)
            self.assertEqual(rows[0]["Distance"], "Sprint")
            self.assertEqual(
                (data / "men_chrono_pred.csv").read_bytes(),
                (data / "men_chrono.csv").read_bytes(),
            )
            with (data / "men_chrono_elevation.csv").open(
                newline="", encoding="utf-8"
            ) as handle:
                elevated = list(csv.DictReader(handle))
            self.assertEqual(elevated[0]["Elevation"], "200")

    def test_validates_and_renders_individual_post(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_prediction(root / "men_position_probabilities.json")
            rendered = render_post(
                date(2026, 11, 27),
                (IndividualEvent(gender="men", distance="Sprint", technique="C"),),
                root,
            )
            self.assertIn("Cross-Country Race Picks", rendered)
            self.assertIn("men_position_probabilities", rendered)
            output = root / "content" / "post.md"
            write_post(output, rendered)
            self.assertEqual(output.read_text(encoding="utf-8"), rendered)

    def test_rejects_invalid_probability_artifact(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_prediction(root / "men_position_probabilities.json", invalid=True)
            with self.assertRaisesRegex(IndividualPublicationError, "out-of-range"):
                render_post(
                    date(2026, 11, 27),
                    (IndividualEvent(gender="men", distance="Sprint", technique="C"),),
                    root,
                )


if __name__ == "__main__":
    unittest.main()
