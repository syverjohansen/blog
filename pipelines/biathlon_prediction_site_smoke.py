"""Render and inspect one generated Biathlon prediction post with production Hugo assets."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any


DATATABLE = re.compile(r'\{\{<\s*biathlon/datatable\s+"([^"]+)"\s*>\}\}')
TITLE = re.compile(r'^title:\s*"([^"]+)"\s*$', re.MULTILINE)


class BiathlonSiteSmokeError(RuntimeError):
    """The generated post cannot be rendered safely."""


def _write_json(path: Path, payload: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def render_prediction_post(
    *,
    repository_root: Path,
    post: Path,
    destination: Path,
    report_path: Path,
    hugo_binary: str | None = None,
) -> dict[str, Any]:
    repository_root = repository_root.expanduser().resolve()
    post = post.expanduser().resolve()
    content_root = (repository_root / "content").resolve()
    try:
        post_relative = post.relative_to(content_root)
    except ValueError as error:
        raise BiathlonSiteSmokeError("post must be inside the repository content root") from error
    if not post.is_file():
        raise BiathlonSiteSmokeError(f"generated post is missing: {post}")

    markdown = post.read_text(encoding="utf-8")
    references = DATATABLE.findall(markdown)
    title_match = TITLE.search(markdown)
    if not title_match or not references:
        raise BiathlonSiteSmokeError("generated post is missing its title or data tables")
    if len(references) != len(set(references)):
        raise BiathlonSiteSmokeError("generated post contains duplicate data tables")
    for reference in references:
        data_file = repository_root / "data" / f"{reference}.json"
        try:
            payload = json.loads(data_file.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise BiathlonSiteSmokeError(f"cannot read table data: {data_file}") from error
        if not isinstance(payload, dict) or not payload.get("headers") or not payload.get("rows"):
            raise BiathlonSiteSmokeError(f"table data is empty or malformed: {data_file}")

    hugo = hugo_binary or shutil.which("hugo")
    if not hugo:
        raise BiathlonSiteSmokeError("hugo is required for the prediction-site smoke")
    destination = destination.expanduser().resolve()
    with tempfile.TemporaryDirectory(prefix="biathlon-prediction-site-") as temporary:
        temporary_root = Path(temporary)
        isolated_content = temporary_root / "content"
        isolated_static = temporary_root / "static"
        isolated_post = isolated_content / post_relative
        isolated_post.parent.mkdir(parents=True)
        isolated_static.mkdir()
        shutil.copy2(post, isolated_post)
        overlay = temporary_root / "smoke.toml"
        overlay.write_text(
            f'contentDir = "{isolated_content.as_posix()}"\n'
            f'staticDir = "{isolated_static.as_posix()}"\n'
            'disableKinds = ["home", "section", "taxonomy", "term", "RSS", '
            '"sitemap", "robotsTXT", "404"]\n',
            encoding="utf-8",
        )
        completed = subprocess.run(
            [
                hugo,
                "--source", str(repository_root),
                "--config", f"config.toml,{overlay}",
                "--destination", str(destination),
                "--cleanDestinationDir",
                "--noBuildLock",
                "--buildFuture",
                "--panicOnWarning",
                "--logLevel", "warn",
            ],
            cwd=repository_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
        if completed.returncode != 0:
            raise BiathlonSiteSmokeError(
                "targeted Hugo render failed:\n" + completed.stdout.strip()
            )

    rendered = destination / post_relative.with_suffix("") / "index.html"
    if not rendered.is_file():
        raise BiathlonSiteSmokeError(f"rendered prediction page is missing: {rendered}")
    html = rendered.read_text(encoding="utf-8")
    if title_match.group(1) not in html:
        raise BiathlonSiteSmokeError("rendered page is missing the prediction title")
    rendered_tables = set(re.findall(r'id="(table-[^"]+)"', html))
    if len(rendered_tables) != len(references):
        raise BiathlonSiteSmokeError(
            f"rendered {len(rendered_tables)} tables; expected {len(references)}"
        )
    for reference in references:
        if not any(reference.rsplit("/", 1)[-1] in table for table in rendered_tables):
            raise BiathlonSiteSmokeError(f"rendered page is missing table: {reference}")

    result = {
        "status": "passed",
        "post": str(post_relative),
        "rendered_page": str(rendered.relative_to(destination)),
        "tables": references,
        "bytes": rendered.stat().st_size,
    }
    _write_json(report_path, result)
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository-root", required=True, type=Path)
    parser.add_argument("--post", required=True, type=Path)
    parser.add_argument("--destination", required=True, type=Path)
    parser.add_argument("--report", required=True, type=Path)
    arguments = parser.parse_args()
    render_prediction_post(
        repository_root=arguments.repository_root,
        post=arguments.post,
        destination=arguments.destination,
        report_path=arguments.report,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
