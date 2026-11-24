from __future__ import annotations

import csv
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

import fantasyxc_roster_digest as digest


class FantasyXCRosterDigestTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.schedule = self.root / "weekends.csv"
        with self.schedule.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(
                handle,
                fieldnames=[
                    "Date",
                    "Startlist",
                    "Sex",
                    "City",
                    "Distance",
                    "Technique",
                    "MS",
                ],
            )
            writer.writeheader()
            writer.writerows(
                [
                    {
                        "Date": "11/27/2026",
                        "Startlist": "https://fis.example/results?raceid=100",
                        "Sex": "L",
                        "City": "Ruka",
                        "Distance": "10",
                        "Technique": "C",
                        "MS": "0",
                    },
                    {
                        "Date": "11/27/2026",
                        "Startlist": "https://fis.example/results?raceid=101",
                        "Sex": "M",
                        "City": "Ruka",
                        "Distance": "20",
                        "Technique": "F",
                        "MS": "1",
                    },
                ]
            )
        self.roster = {
            "schema_version": "nordicnumbers.cross-country-announced-rosters/v2",
            "status": "automated",
            "logical_date": "2026-11-27",
            "entries": [
                {
                    "canonical_name": "Astrid Oeyre Slind",
                    "canonical_nation": "Norway",
                    "status": "confirmed",
                    "race_ids": ["100"],
                },
                {
                    "canonical_name": "Iivo Niskanen",
                    "canonical_nation": "Finland",
                    "status": "excluded",
                    "race_ids": ["*"],
                },
            ],
            "unresolved": [
                {
                    "canonical_name": "Maybe Athlete",
                    "canonical_nation": "Sweden",
                    "race_ids": ["101"],
                    "reason": "candidate failed automatic confidence or identity policy",
                    "source_url": "https://x.com/FantasyXC/status/1",
                }
            ],
        }

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def test_render_groups_country_event_and_fallback(self) -> None:
        events, venue = digest.load_events(
            self.schedule, digest.date.fromisoformat("2026-11-27")
        )
        text, html, metadata = digest.render_digest(self.roster, events, venue)

        self.assertIn("Norway\n  Women — 10 km classic: Astrid Oeyre Slind", text)
        self.assertIn("Finland\n  Entire weekend: Iivo Niskanen", text)
        self.assertIn(
            "Sweden | Men — 20 km freestyle mass start | Maybe Athlete", text
        )
        self.assertIn("Still up in the air", html)
        self.assertIn("historical/quota model", html)
        self.assertEqual(2, metadata["accepted_decisions"])
        self.assertEqual(1, metadata["unresolved"])
        self.assertEqual(64, len(metadata["digest_sha256"]))

    def test_render_digest_is_stable(self) -> None:
        events, venue = digest.load_events(
            self.schedule, digest.date.fromisoformat("2026-11-27")
        )
        first = digest.render_digest(self.roster, events, venue)[2]["digest_sha256"]
        copied = json.loads(json.dumps(self.roster, sort_keys=True))
        second = digest.render_digest(copied, events, venue)[2]["digest_sha256"]
        self.assertEqual(first, second)

    @patch("fantasyxc_roster_digest.smtplib.SMTP_SSL")
    def test_send_uses_authenticated_tls(self, smtp_ssl: MagicMock) -> None:
        smtp = smtp_ssl.return_value.__enter__.return_value
        digest.send_digest(
            sender="sender@example.com",
            recipient="recipient@example.com",
            password="app-password",
            subject="Roster",
            text_body="Text",
            html_body="<p>HTML</p>",
        )

        smtp_ssl.assert_called_once()
        smtp.login.assert_called_once_with("sender@example.com", "app-password")
        message = smtp.send_message.call_args.args[0]
        self.assertEqual("recipient@example.com", message["To"])
        self.assertTrue(message.is_multipart())


if __name__ == "__main__":
    unittest.main()
