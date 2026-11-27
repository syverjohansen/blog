#!/usr/bin/env python3
"""Build and target-render one combined daily Race Picks post."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import tempfile
from datetime import UTC, date, datetime
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
FRONT_MATTER_DATE = re.compile(r"^date:\s*(?P<value>\S+)\s*$", re.MULTILINE)
HEADING = re.compile(r"^(?P<level>#{2,3})\s+(?P<label>.+?)\s*$")


def fragment_path(repository_root: Path, logical_date: date, name: str) -> Path:
    return (
        repository_root
        / "race-picks/fragments"
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


def _predicted_winners(repository_root: Path, markdown: str) -> list[dict[str, object]]:
    """Extract the highest win probability from each event table for previews."""
    winners: list[dict[str, object]] = []
    sport = "Race Picks"
    event = "Event"
    for line in markdown.splitlines():
        heading = HEADING.match(line)
        if heading:
            if heading.group("level") == "##":
                sport = heading.group("label")
            else:
                event = heading.group("label")
            continue
        shortcode = SHORTCODE.search(line)
        if not shortcode:
            continue
        reference = shortcode.group("reference")
        path = repository_root / "data" / f"{reference}.json"
        document = json.loads(path.read_text(encoding="utf-8"))
        if not isinstance(document, dict):
            continue
        headers = document.get("headers")
        rows = document.get("rows")
        if not isinstance(headers, list) or not isinstance(rows, list):
            continue
        competitor_header = next(
            (name for name in ("Skier", "Team", "Team_Name", "Nation") if name in headers),
            None,
        )
        win_header = next((name for name in ("Win", "Win_Prob") if name in headers), None)
        if competitor_header is None or win_header is None:
            continue
        competitor_index = headers.index(competitor_header)
        nation_index = headers.index("Nation") if "Nation" in headers else None
        win_index = headers.index(win_header)
        candidates: list[tuple[float, list[object]]] = []
        for row in rows:
            required_indexes = [competitor_index, win_index]
            if nation_index is not None:
                required_indexes.append(nation_index)
            if not isinstance(row, list) or len(row) <= max(required_indexes):
                continue
            try:
                probability = float(row[win_index])
            except (TypeError, ValueError):
                continue
            candidates.append((probability, row))
        if not candidates:
            continue
        probability, winner = max(candidates, key=lambda candidate: candidate[0])
        if 0 <= probability <= 1:
            probability *= 100
        nation = ""
        if nation_index is not None and nation_index != competitor_index:
            nation = str(winner[nation_index])
        winners.append(
            {
                "sport": sport,
                "event": event,
                "winner": str(winner[competitor_index]),
                "nation": nation,
                "win_probability": round(probability, 1),
            }
        )
    return winners


def _winner_front_matter(winners: list[dict[str, object]]) -> str:
    if not winners:
        return ""
    names = [str(winner["winner"]) for winner in winners]
    if len(names) == 1:
        summary = names[0]
    elif len(names) == 2:
        summary = " and ".join(names)
    else:
        summary = ", ".join(names[:-1]) + f", and {names[-1]}"
    lines = [
        f"description: {json.dumps(f'Predicted winners: {summary}.', ensure_ascii=False)}",
        "predicted_winners:",
    ]
    for winner in winners:
        lines.extend(
            [
                f"  - sport: {json.dumps(winner['sport'], ensure_ascii=False)}",
                f"    event: {json.dumps(winner['event'], ensure_ascii=False)}",
                f"    winner: {json.dumps(winner['winner'], ensure_ascii=False)}",
                f"    nation: {json.dumps(winner['nation'], ensure_ascii=False)}",
                f"    win_probability: {winner['win_probability']}",
            ]
        )
    return "\n".join(lines) + "\n"


def _safe_publication_time(post: Path, current_time: datetime) -> datetime:
    if post.is_file():
        match = FRONT_MATTER_DATE.search(post.read_text(encoding="utf-8"))
        if match:
            try:
                previous = datetime.fromisoformat(match.group("value"))
            except ValueError:
                previous = None
            if previous is not None and previous.tzinfo is not None and previous <= current_time:
                return previous
    return current_time


def build_combined_post(
    repository_root: Path,
    logical_date: date,
    *,
    publication_time: datetime | None = None,
) -> dict[str, object]:
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

    date_key = logical_date.strftime("%Y%m%d")
    target = repository_root / "content/post/race-picks" / f"{date_key}.md"
    visible_time = _safe_publication_time(
        target, publication_time or datetime.now(UTC)
    )
    body = (
        "*See [Race Picks Methodology](/post/methods/race-picks/) for details on how these predictions are generated.*\n\n"
        + "\n\n".join(sections)
        + "\n"
    )
    references = _validate_references(repository_root, body)
    winners = _predicted_winners(repository_root, body)
    markdown = (
        "---\n"
        f'title: "Race Picks for {logical_date.strftime("%m/%d/%Y")}"\n'
        f"date: {visible_time.astimezone(UTC).isoformat(timespec='seconds')}\n"
        "draft: false\n"
        'tags: ["predictions", "skiing", "race-picks"]\n'
        + _winner_front_matter(winners)
        + "---\n\n"
        + body
    )
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
            "--buildFuture",
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
