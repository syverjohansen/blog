import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from pipelines.sport_site_smoke import (
    EXPECTED_ROUTES,
    SportSiteSmokeError,
    run_smoke,
)


class SportSiteSmokeTest(unittest.TestCase):
    def _blog(self, root: Path) -> Path:
        blog = root / "blog"
        (blog / "content/skijump").mkdir(parents=True)
        (blog / "content/skijump/_index.md").write_text("---\ntitle: Test\n---\n")
        (blog / "config.toml").write_text('theme = "white"\n')
        return blog

    def test_renders_only_bounded_sport_content_and_checks_every_route(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            blog = self._blog(Path(directory))

            def render(command: list[str], **_: object) -> subprocess.CompletedProcess[str]:
                destination = Path(command[command.index("--destination") + 1])
                for route in EXPECTED_ROUTES:
                    path = destination / "skijump" / route
                    path.parent.mkdir(parents=True, exist_ok=True)
                    path.write_text("rendered")
                config = command[command.index("--config") + 1]
                self.assertIn("smoke.toml", config)
                self.assertIn("--panicOnWarning", command)
                return subprocess.CompletedProcess(command, 0, stdout="ok")

            with patch("pipelines.sport_site_smoke.shutil.which", return_value="/bin/hugo"), patch(
                "pipelines.sport_site_smoke.subprocess.run", side_effect=render
            ):
                result = run_smoke(blog, "skijump")

            self.assertEqual(result["status"], "passed")
            self.assertEqual(result["route_count"], len(EXPECTED_ROUTES))

    def test_fails_closed_when_a_route_is_not_rendered(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            blog = self._blog(Path(directory))
            completed = subprocess.CompletedProcess(["hugo"], 0, stdout="ok")
            with patch("pipelines.sport_site_smoke.shutil.which", return_value="/bin/hugo"), patch(
                "pipelines.sport_site_smoke.subprocess.run", return_value=completed
            ):
                with self.assertRaisesRegex(SportSiteSmokeError, "missing"):
                    run_smoke(blog, "skijump")


if __name__ == "__main__":
    unittest.main()
