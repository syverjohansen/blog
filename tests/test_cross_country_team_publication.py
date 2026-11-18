import csv
import json
import tempfile
import unittest
from datetime import date
from pathlib import Path

from pipelines.cross_country_team_publication import (
    TeamEvent,
    TeamPublicationError,
    detect_team_events,
    materialize_event_inputs,
    render_post,
    write_post,
)


def _write_schedule(path: Path, rows: list[dict[str, str]]) -> None:
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=("Date", "Distance", "Sex"))
        writer.writeheader()
        writer.writerows(rows)


def _write_artifacts(root: Path, prefix: str, *, invalid: bool = False) -> None:
    points = {
        "headers": ["Team", "Leg 1", "Leg 2", "Expected Points"],
        "rows": [["Norway I", "A", "B", 180]],
    }
    probabilities = {
        "headers": ["Team", "Leg 1", "Leg 2", "Win", "Podium", "Top 5", "Top 10"],
        "rows": [["Norway I", "A", "B", 1.1 if invalid else 0.4, 0.7, 0.9, 1.0]],
    }
    (root / f"{prefix}.json").write_text(json.dumps(points), encoding="utf-8")
    (root / f"{prefix}_position_probabilities.json").write_text(
        json.dumps(probabilities), encoding="utf-8"
    )


class CrossCountryTeamPublicationTests(unittest.TestCase):
    def test_detects_only_supported_team_events_for_logical_date(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            schedule = Path(directory) / "races.csv"
            _write_schedule(
                schedule,
                [
                    {"Date": "12/11/2026", "Distance": "Ts", "Sex": "M"},
                    {"Date": "12/11/2026", "Distance": "Ts", "Sex": "L"},
                    {"Date": "12/11/2026", "Distance": "Sprint", "Sex": "M"},
                    {"Date": "12/12/2026", "Distance": "Rel", "Sex": "M"},
                ],
            )
            self.assertEqual(
                detect_team_events(schedule, date(2026, 12, 11)),
                (
                    TeamEvent(event_type="team_sprint", gender="ladies"),
                    TeamEvent(event_type="team_sprint", gender="men"),
                ),
            )

    def test_renders_validated_post_and_writes_atomically(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_artifacts(root, "men_team_sprint")
            rendered = render_post(
                date(2026, 12, 11),
                (TeamEvent(event_type="team_sprint", gender="men"),),
                root,
            )
            self.assertIn("Men's Team Sprint", rendered)
            self.assertIn("men_team_sprint_position_probabilities", rendered)
            output = root / "content" / "post.md"
            write_post(output, rendered)
            self.assertEqual(output.read_text(encoding="utf-8"), rendered)

    def test_materializes_one_combined_input_per_event_type(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            schedule = root / "races.csv"
            _write_schedule(
                schedule,
                [
                    {"Date": "12/11/2026", "Distance": "Ts", "Sex": "M"},
                    {"Date": "12/11/2026", "Distance": "Ts", "Sex": "L"},
                    {"Date": "12/12/2026", "Distance": "Rel", "Sex": "M"},
                ],
            )
            outputs = materialize_event_inputs(
                schedule, date(2026, 12, 11), root / "inputs"
            )
            self.assertEqual(outputs, (root / "inputs/team_sprint.csv",))
            with outputs[0].open(newline="", encoding="utf-8") as handle:
                rows = list(csv.DictReader(handle))
            self.assertEqual([row["Sex"] for row in rows], ["M", "L"])

    def test_rejects_invalid_probability_artifact(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            _write_artifacts(root, "men_team_sprint", invalid=True)
            with self.assertRaisesRegex(TeamPublicationError, "out-of-range"):
                render_post(
                    date(2026, 12, 11),
                    (TeamEvent(event_type="team_sprint", gender="men"),),
                    root,
                )

    def test_refuses_post_without_team_events(self) -> None:
        with self.assertRaisesRegex(TeamPublicationError, "without events"):
            render_post(date(2026, 12, 11), (), Path("unused"))


if __name__ == "__main__":
    unittest.main()
