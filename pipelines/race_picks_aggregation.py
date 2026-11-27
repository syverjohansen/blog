#!/usr/bin/env python3
"""Build and target-render one combined daily Race Picks post."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import tempfile
from datetime import date
from pathlib import Path


class RacePicksAggregationError(RuntimeError):
    """Raised when prediction fragments cannot form a safe combined post."""


FRAGMENT_GROUPS = (
    ("alpine", ("alpine",)),
    ("cross-country", ("cross-country-individual", "cross-country-team")),
    ("skijump", ("ski-jumping",)),
    ("nordic-combined", ("nordic-combined",)),
    ("biathlon", ("biathlon",)),
)
SHORTCODE = re.compile(
    r'\{\{<\s+(?P<sport>[a-z-]+)/datatable\s+"(?P<reference>[^"]+)"\s*>\}\}'
)
METHODOLOGY = re.compile(r"^\*See \[Race Picks Methodology\].*\*$")


def fragment_path(repository_root: Path, logical_date: date, name: str) -> Path:
    return (
        repository_root
        / "data/race-picks/fragments"
        / logical_date.strftime("%Y%m%d")
        / f"{name}.md"
    )


def _body(path: Path) -> list[str]:
    lines = path.read_text(encoding="utf-8").splitlines()
    if not lines or lines[0].strip() != "---":
        raise RacePicksAggregationError(f"fragment has no front matter: {path}")
    try:
        end = lines.index("---", 1)
    except ValueError as exc:
        raise RacePicksAggregationError(f"fragment has invalid front matter: {path}") from exc
    body = [line for line in lines[end + 1 :] if not METHODOLOGY.match(line.strip())]
    while body and not body[0].strip():
        body.pop(0)
    while body and not body[-1].strip():
        body.pop()
    if not body or not body[0].startswith("## "):
        raise RacePicksAggregationError(f"fragment has no sport section: {path}")
    return body


def _validate_references(repository_root: Path, markdown: str) -> list[str]:
    references: list[str] = []
    for match in SHORTCODE.finditer(markdown):
        reference = match.group("reference")
        path = repository_root / "data" / f"{reference}.json"
        if not path.is_file():
            raise RacePicksAggregationError(f"referenced prediction data is missing: {path}")
        try:
            document = json.loads(path.read_text(encoding="utf-8"))
        except json.JSONDecodeError as exc:
            raise RacePicksAggregationError(f"invalid prediction JSON: {path}") from exc
        if not isinstance(document, (dict, list)) or not document:
            raise RacePicksAggregationError(f"prediction data is empty: {path}")
        references.append(reference)
    if not references:
        raise RacePicksAggregationError("combined Race Picks post has no prediction tables")
    if len(references) != len(set(references)):
        raise RacePicksAggregationError("combined Race Picks post repeats a prediction table")
    return references


def build_combined_post(repository_root: Path, logical_date: date) -> dict[str, object]:
    repository_root = repository_root.resolve()
    sections: list[str] = []
    included: list[str] = []
    for _, fragment_names in FRAGMENT_GROUPS:
        group_bodies: list[list[str]] = []
        for name in fragment_names:
            path = fragment_path(repository_root, logical_date, name)
            if path.is_file():
                group_bodies.append(_body(path))
                included.append(name)
        if not group_bodies:
            continue
        merged = group_bodies[0]
        for additional in group_bodies[1:]:
            # Individual and team Cross Country fragments share one public section.
            merged.extend(["", *additional[1:]])
        sections.append("\n".join(merged).strip())
    if not sections:
        raise RacePicksAggregationError(
            f"no prediction fragments exist for {logical_date.isoformat()}"
        )

    markdown = (
        "---\n"
        f'title: "Race Picks for {logical_date.strftime("%m/%d/%Y")}"\n'
        f"date: {logical_date.isoformat()}T06:00:00+00:00\n"
        "draft: false\n"
        'tags: ["predictions", "skiing", "race-picks"]\n'
        "---\n\n"
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details on how these predictions are generated.*\n\n"
        + "\n\n".join(sections)
        + "\n"
    )
    references = _validate_references(repository_root, markdown)
    date_key = logical_date.strftime("%Y%m%d")
    target = repository_root / "content/post/race-picks" / f"{date_key}.md"
    target.parent.mkdir(parents=True, exist_ok=True)
    previous = target.read_text(encoding="utf-8") if target.is_file() else None
    with tempfile.NamedTemporaryFile(
        mode="w", encoding="utf-8", dir=target.parent, delete=False
    ) as handle:
        handle.write(markdown)
        temporary = Path(handle.name)
    temporary.replace(target)

    removed: list[str] = []
    for prefix in (
        "biathlon",
        "ski-jumping",
        "nordic-combined",
        "cross-country-individual",
        "cross-country-team",
    ):
        legacy = target.parent / f"{prefix}-{date_key}.md"
        if legacy.is_file():
            legacy.unlink()
            removed.append(str(legacy.relative_to(repository_root)))
    return {
        "status": "unchanged" if previous == markdown and not removed else "prepared",
        "logical_date": logical_date.isoformat(),
        "post": str(target.relative_to(repository_root)),
        "fragments": included,
        "references": references,
        "removed_legacy_posts": removed,
    }


def render_combined_post(
    repository_root: Path, logical_date: date, destination: Path
) -> dict[str, object]:
    repository_root = repository_root.resolve()
    hugo = shutil.which("hugo")
    if not hugo:
        raise RacePicksAggregationError("hugo is required to render Race Picks")
    date_key = logical_date.strftime("%Y%m%d")
    post = repository_root / "content/post/race-picks" / f"{date_key}.md"
    if not post.is_file():
        raise RacePicksAggregationError(f"combined Race Picks post is missing: {post}")
    references = _validate_references(repository_root, post.read_text(encoding="utf-8"))

    with tempfile.TemporaryDirectory(prefix="combined-race-picks-") as directory:
        root = Path(directory)
        content_root = root / "content/post/race-picks"
        data_root = root / "data"
        static_root = root / "static"
        content_root.mkdir(parents=True)
        data_root.mkdir()
        static_root.mkdir()
        shutil.copy2(post, content_root / post.name)
        for reference in references:
            source = repository_root / "data" / f"{reference}.json"
            target = data_root / f"{reference}.json"
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
        override = root / "combined.toml"
        override.write_text(
            f'contentDir = "{(root / "content").as_posix()}"\n'
            f'dataDir = "{data_root.as_posix()}"\n'
            f'staticDir = "{static_root.as_posix()}"\n'
            'disableKinds = ["home", "section", "taxonomy", "term", "RSS", '
            '"sitemap", "robotsTXT", "404"]\n',
            encoding="utf-8",
        )
        destination = destination.resolve()
        command = [
            hugo,
            "--source",
            str(repository_root),
            "--config",
            f"{repository_root / 'config.toml'},{override}",
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
            command, text=True, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, check=False,
        )
        if completed.returncode:
            raise RacePicksAggregationError(
                f"combined Race Picks Hugo render failed:\n{completed.stdout.strip()}"
            )
    rendered = destination / "post/race-picks" / date_key / "index.html"
    if not rendered.is_file() or rendered.stat().st_size == 0:
        raise RacePicksAggregationError(f"combined Race Picks page was not rendered: {rendered}")
    html = rendered.read_text(encoding="utf-8")
    missing = [reference for reference in references if reference.replace("/", "") not in html]
    if missing:
        raise RacePicksAggregationError(
            f"rendered Race Picks page is missing tables: {missing}"
        )
    return {"status": "passed", "page": str(rendered), "references": references}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    for name in ("build", "render"):
        command = subparsers.add_parser(name)
        command.add_argument("--repository-root", type=Path, default=Path.cwd())
        command.add_argument("--logical-date", type=date.fromisoformat, required=True)
        command.add_argument("--report", type=Path, required=True)
        if name == "render":
            command.add_argument("--destination", type=Path, required=True)
    args = parser.parse_args()
    if args.command == "build":
        report = build_combined_post(args.repository_root, args.logical_date)
    else:
        report = render_combined_post(
            args.repository_root, args.logical_date, args.destination
        )
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
