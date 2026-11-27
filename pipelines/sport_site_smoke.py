#!/usr/bin/env python3
"""Render only one sport's Hugo routes as a publication smoke gate.

Site-data workflows already validate every generated JSON/CSV artifact before
this command runs.  Rendering the entire historical site adds no extra data
validation and can take longer than the workflow timeout.  This smoke instead
copies the small, version-controlled content tree for one sport into an
isolated Hugo content directory and renders every route in that tree with the
production layouts and theme.
"""

from __future__ import annotations

import argparse
import json
import shutil
import subprocess
import tempfile
from pathlib import Path


class SportSiteSmokeError(RuntimeError):
    """Raised when the bounded Hugo publication smoke fails."""


SPORTS = ("alpine", "biathlon", "cross-country", "nordic-combined", "skijump")
EXPECTED_ROUTES = (
    "index.html",
    "all-elo/index.html",
    "athletes/index.html",
    "athletes/skier/index.html",
    "elo/index.html",
    "head2head/index.html",
    "races/index.html",
    "races/race/index.html",
    "races/season/index.html",
    "ranks/index.html",
    "skiers/skier/index.html",
)


def run_smoke(blog_root: Path, sport: str) -> dict[str, object]:
    blog_root = blog_root.resolve()
    if sport not in SPORTS:
        raise SportSiteSmokeError(f"unsupported sport: {sport}")

    hugo = shutil.which("hugo")
    if not hugo:
        raise SportSiteSmokeError("hugo is required for the site smoke")

    main_config = blog_root / "config.toml"
    source = blog_root / "content" / sport
    if not main_config.is_file():
        raise SportSiteSmokeError(f"missing Hugo config: {main_config}")
    if not source.is_dir():
        raise SportSiteSmokeError(f"missing sport content: {source}")

    with tempfile.TemporaryDirectory(prefix=f"{sport}-site-smoke-") as directory:
        root = Path(directory)
        content_root = root / "content"
        static_root = root / "static"
        destination = root / "site"
        content_root.mkdir()
        static_root.mkdir()
        shutil.copytree(source, content_root / sport)

        overlay = root / "smoke.toml"
        overlay.write_text(
            f'contentDir = "{content_root.as_posix()}"\n'
            f'staticDir = "{static_root.as_posix()}"\n'
            'disableKinds = ["home", "taxonomy", "term", "RSS", '
            '"sitemap", "robotsTXT", "404"]\n',
            encoding="utf-8",
        )
        command = [
            hugo,
            "--source",
            str(blog_root),
            "--config",
            f"{main_config},{overlay}",
            "--destination",
            str(destination),
            "--cleanDestinationDir",
            "--noBuildLock",
            "--panicOnWarning",
            "--logLevel",
            "warn",
            "--minify",
        ]
        completed = subprocess.run(
            command,
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if completed.returncode != 0:
            raise SportSiteSmokeError(
                f"bounded {sport} Hugo render failed:\n{completed.stdout.strip()}"
            )

        rendered = []
        for route in EXPECTED_ROUTES:
            path = destination / sport / route
            if not path.is_file() or path.stat().st_size == 0:
                raise SportSiteSmokeError(f"expected rendered route is missing: {path}")
            rendered.append(f"{sport}/{route}")

        return {
            "status": "passed",
            "sport": sport,
            "rendered_routes": rendered,
            "route_count": len(rendered),
        }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--blog-root", type=Path, default=Path.cwd())
    parser.add_argument("--sport", choices=SPORTS, required=True)
    args = parser.parse_args()
    print(json.dumps(run_smoke(args.blog_root, args.sport), sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
