import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from daily_results_digest import (
    collect,
    render,
    sync_issues,
    accepted_provisionals,
    exclude_automatic_aliases,
)


class DailyResultsDigestTests(unittest.TestCase):
    def test_collects_every_unresolved_and_provisional_identity(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "report.json").write_text(
                json.dumps(
                    {
                        "status": "blocked",
                        "review_required": [
                            {
                                "sex": "M",
                                "fis_code": "3270010",
                                "fis_name": "Thomas",
                                "status": "provisional_newcomer",
                            }
                        ],
                    }
                )
            )
            with (root / "unresolved_identities.csv").open("w") as handle:
                handle.write(
                    "event_sex,fis_code,fis_name,reason\nMixed,1234567,Unmatched,ambiguous\n"
                )
            with (root / "identity_map.csv").open("w") as handle:
                handle.write(
                    "sex,fis_code,fis_name,identity_match_method\nM,3270010,Thomas,newcomer_fis_identity\n"
                )
            reviews, reports, _ = collect([root])
            self.assertEqual(len(reviews), 2)
            self.assertEqual({r["fis_code"] for r in reviews}, {"3270010", "1234567"})
            self.assertEqual(reports[0]["status"], "blocked")

    def test_missing_evidence_is_not_reported_as_clean_match(self):
        _, reports, _ = collect([Path("/nonexistent/daily-results-evidence")])
        self.assertEqual(reports[0]["status"], "evidence_not_produced")
        text = render("biathlon", "2026-11-27", "failure", "run", [], reports, [])
        self.assertIn("failure", text)
        self.assertIn("evidence_not_produced", text)

    @patch("daily_results_digest.api")
    def test_closed_reviews_are_not_reopened_and_open_reviews_survive_noop(self, api):
        closed = {
            "number": 1,
            "state": "closed",
            "title": "Reviewed debut",
            "html_url": "closed",
            "body": "<!-- athlete-identity:cross-country/M/3270010 -->",
        }
        opened = {
            "number": 2,
            "state": "open",
            "title": "Still unmatched",
            "html_url": "open",
            "body": "<!-- athlete-identity:cross-country/L/3426626 -->",
        }
        api.side_effect = [[[{"name": "athlete-identity"}]], [[closed, opened]]]
        issues = sync_issues(
            "owner/repo",
            "cross-country",
            [{"sex": "M", "fis_code": "3270010"}],
            set(),
            "run",
        )
        self.assertEqual([r["number"] for r in issues], [2])
        self.assertEqual(api.call_count, 2)

    @patch("daily_results_digest.api")
    def test_reviewed_alias_closes_existing_issue(self, api):
        issue = {
            "number": 1,
            "state": "open",
            "body": "<!-- athlete-identity:cross-country/M/3270010 -->",
        }
        api.side_effect = [[[{"name": "athlete-identity"}]], [[issue]], {}]
        issues = sync_issues(
            "owner/repo", "cross-country", [], {("M", "3270010")}, "run"
        )
        self.assertEqual(issues, [])
        self.assertEqual(api.call_args.kwargs["payload"]["state"], "closed")

    def test_automatic_ibu_alias_does_not_silently_close_review(self):
        with tempfile.TemporaryDirectory() as directory:
            aliases = Path(directory) / "aliases.csv"
            aliases.write_text(
                "IBUId,ID,reason\nBTNEW,5551,automatic official IBU debut identity\nBTREVIEWED,42,verified name change\n"
            )
            matched = exclude_automatic_aliases(
                {("L", "BTNEW"), ("M", "BTREVIEWED")}, aliases
            )
            self.assertEqual(matched, {("M", "BTREVIEWED")})

    def test_existing_provisional_ids_remain_visible_after_noop(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "men_chrono.csv").write_text(
                "ID,Skier\n5553,Historical\n3270010,Thomas\n3999999,Debut\n"
            )
            alias = root / "aliases.csv"
            alias.write_text("sex,fis_code\nM,3270010\n")
            reviews, reviewed = accepted_provisionals(root, alias)
            self.assertEqual([r["fis_code"] for r in reviews], ["3999999"])
            self.assertIn(("M", "3270010"), reviewed)


if __name__ == "__main__":
    unittest.main()
