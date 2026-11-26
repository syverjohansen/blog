from __future__ import annotations

import tempfile
import unittest
from datetime import UTC, date, datetime
from pathlib import Path

from pipelines.biathlon_prediction_publication import _post, _safe_publication_time


class BiathlonPredictionPublicationDateTests(unittest.TestCase):
    def test_post_uses_generation_time_not_future_race_time(self) -> None:
        generated = datetime(2026, 11, 26, 21, 5, 13, tzinfo=UTC)
        post = _post(date(2026, 11, 27), [], generated).decode()
        self.assertIn('title: "Biathlon Race Picks for 11/27/2026"', post)
        self.assertIn("date: 2026-11-26T21:05:13+00:00", post)
        self.assertNotIn("date: 2026-11-27", post)

    def test_rerun_preserves_a_prior_visible_publication_time(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            post = Path(temporary) / "post.md"
            post.write_text(
                "---\ndate: 2026-11-26T21:05:13+00:00\n---\n",
                encoding="utf-8",
            )
            current = datetime(2026, 11, 26, 22, 0, 0, tzinfo=UTC)
            self.assertEqual(
                _safe_publication_time(post, current),
                datetime(2026, 11, 26, 21, 5, 13, tzinfo=UTC),
            )

    def test_future_legacy_timestamp_is_corrected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            post = Path(temporary) / "post.md"
            post.write_text(
                "---\ndate: 2026-11-27T06:00:00+00:00\n---\n",
                encoding="utf-8",
            )
            current = datetime(2026, 11, 26, 22, 0, 0, tzinfo=UTC)
            self.assertEqual(_safe_publication_time(post, current), current)


if __name__ == "__main__":
    unittest.main()
