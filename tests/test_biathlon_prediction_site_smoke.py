from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from pipelines.biathlon_prediction_site_smoke import (
    BiathlonSiteSmokeError,
    render_prediction_post,
)


class BiathlonPredictionSiteSmokeTests(unittest.TestCase):
    def _fixture(self, root: Path) -> tuple[Path, Path]:
        (root / "content/post/race-picks").mkdir(parents=True)
        (root / "data/biathlon/drafts/race-picks/20261127").mkdir(parents=True)
        post = root / "content/post/race-picks/biathlon-20261127.md"
        post.write_text(
            '---\ntitle: "Biathlon Race Picks for 11/27/2026"\n'
            'date: 2026-11-27T06:00:00+00:00\n---\n'
            '{{< biathlon/datatable '
            '"biathlon/drafts/race-picks/20261127/ladies_position_probabilities" >}}\n',
            encoding="utf-8",
        )
        data = root / "data/biathlon/drafts/race-picks/20261127/ladies_position_probabilities.json"
        data.write_text(
            json.dumps({"headers": ["Name", "Win"], "rows": [["Athlete", 0.5]]}),
            encoding="utf-8",
        )
        return post, data

    def test_targeted_render_checks_the_real_page_and_tables(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            post, _ = self._fixture(root)
            destination = root / "site"

            def fake_run(command: list[str], **_: object) -> mock.Mock:
                self.assertIn("--buildFuture", command)
                config = command[command.index("--config") + 1]
                self.assertIn("smoke.toml", config)
                page = destination / "post/race-picks/biathlon-20261127/index.html"
                page.parent.mkdir(parents=True)
                page.write_text(
                    '<h1>Biathlon Race Picks for 11/27/2026</h1>'
                    '<table id="table-biathlondraftsrace-picks20261127ladies_position_probabilities"></table>',
                    encoding="utf-8",
                )
                return mock.Mock(returncode=0, stdout="")

            with mock.patch(
                "pipelines.biathlon_prediction_site_smoke.subprocess.run",
                side_effect=fake_run,
            ):
                result = render_prediction_post(
                    repository_root=root,
                    post=post,
                    destination=destination,
                    report_path=root / "report.json",
                    hugo_binary="hugo",
                )
            self.assertEqual(result["status"], "passed")
            self.assertEqual(len(result["tables"]), 1)

    def test_missing_table_data_fails_before_hugo(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            post, data = self._fixture(root)
            data.unlink()
            with self.assertRaisesRegex(BiathlonSiteSmokeError, "cannot read table data"):
                render_prediction_post(
                    repository_root=root,
                    post=post,
                    destination=root / "site",
                    report_path=root / "report.json",
                    hugo_binary="hugo",
                )


if __name__ == "__main__":
    unittest.main()
