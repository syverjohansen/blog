#!/usr/bin/env python3
"""Validate and target-render one Nordic Numbers sport publication contract."""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Any


SPORT_PAGES = {
    "landing": ("_index.md", "index.html", "Current Elo Scores"),
    "current_elo": ("elo/index.md", "elo/index.html", "dataTableL_chrono"),
    "all_time_elo": (
        "all-elo/index.md",
        "all-elo/index.html",
        "dataTableL_chrono_alltime",
    ),
    "race_archive": ("races/_index.md", "races/index.html", "seasons.json"),
    "rankings": ("ranks/_index.md", "ranks/index.html", "dataTableL_ranks"),
    "athletes": ("athletes/_index.md", "athletes/index.html", "M_all_ids.json"),
}

STATIC_JSON = (
    "L_chrono_current.json",
    "M_chrono_current.json",
    "L_chrono_all_time.json",
    "M_chrono_all_time.json",
    "L_all_ids.json",
    "M_all_ids.json",
    "L_skiers_lookup.json",
    "M_skiers_lookup.json",
    "L/seasons.json",
    "M/seasons.json",
    "L/ranks.json",
    "M/ranks.json",
)


def load_contracts(repo_root: Path) -> dict[str, dict[str, str]]:
    path = repo_root / "pipelines" / "sport_publication_contracts.json"
    contracts = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(contracts, dict) or not contracts:
        raise ValueError(f"Publication contracts must be a non-empty object: {path}")
    return contracts


def load_nonempty_json(path: Path) -> Any:
    if not path.is_file():
        raise FileNotFoundError(f"Required publication data is missing: {path}")
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, (list, dict)) or not value:
        raise ValueError(f"Required publication data is empty: {path}")
    return value


def find_prediction_fixture(repo_root: Path, sport: str) -> tuple[Path, list[str]]:
    pattern = re.compile(
        rf'\{{\{{<\s+{re.escape(sport)}/datatable\s+"([^"]+)"\s*>\}}\}}'
    )
    posts = sorted((repo_root / "content" / "post" / "race-picks").glob("*.md"))
    for post in reversed(posts):
        references = pattern.findall(post.read_text(encoding="utf-8"))
        if references:
            return post, references
    raise FileNotFoundError(f"No published prediction fixture found for {sport}")


def validate_contract(repo_root: Path, sport: str) -> dict[str, Any]:
    contracts = load_contracts(repo_root)
    if sport not in contracts:
        raise ValueError(f"Unknown sport {sport!r}; choose from {sorted(contracts)}")

    content_root = repo_root / "content" / sport
    checked_pages: list[str] = []
    for source, _, _ in SPORT_PAGES.values():
        path = content_root / source
        if not path.is_file():
            raise FileNotFoundError(f"Required publication page is missing: {path}")
        checked_pages.append(str(path.relative_to(repo_root)))

    static_root = repo_root / "static" / "python" / sport / "excel365"
    checked_json: list[str] = []
    for relative in STATIC_JSON:
        path = static_root / relative
        load_nonempty_json(path)
        checked_json.append(str(path.relative_to(repo_root)))

    prediction_post, prediction_refs = find_prediction_fixture(repo_root, sport)
    for reference in prediction_refs:
        path = repo_root / "data" / f"{reference}.json"
        load_nonempty_json(path)
        checked_json.append(str(path.relative_to(repo_root)))

    return {
        "sport": sport,
        "display_name": contracts[sport]["display_name"],
        "automation_status": contracts[sport]["automation_status"],
        "content_pages": checked_pages,
        "static_json": checked_json,
        "prediction_fixture": str(prediction_post.relative_to(repo_root)),
        "prediction_references": prediction_refs,
    }


def _write_prediction_fixture(
    repo_root: Path,
    content_root: Path,
    data_root: Path,
    sport: str,
    source_post: Path,
    references: list[str],
) -> Path:
    target = content_root / "post" / "race-picks" / source_post.name
    target.parent.mkdir(parents=True, exist_ok=True)
    shortcodes = "\n\n".join(
        f'{{{{< {sport}/datatable "{reference}" >}}}}' for reference in references
    )
    target.write_text(
        "---\n"
        f'title: "{sport} publication smoke"\n'
        "date: 2026-01-01T00:00:00Z\n"
        "draft: false\n"
        "---\n\n"
        f"# {sport} prediction publication smoke\n\n{shortcodes}\n",
        encoding="utf-8",
    )
    for reference in references:
        source = repo_root / "data" / f"{reference}.json"
        destination = data_root / f"{reference}.json"
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
    return target


def render_contract(
    repo_root: Path,
    sport: str,
    validation: dict[str, Any],
    destination: Path,
) -> dict[str, Any]:
    hugo = shutil.which("hugo")
    if not hugo:
        raise RuntimeError("hugo is required for publication smoke tests")

    with tempfile.TemporaryDirectory(prefix=f"{sport}-publication-") as tmp:
        tmp_root = Path(tmp)
        content_root = tmp_root / "content"
        data_root = tmp_root / "data"
        static_root = tmp_root / "static"
        shutil.copytree(repo_root / "content" / sport, content_root / sport)
        data_root.mkdir()
        static_root.mkdir()

        source_post = repo_root / validation["prediction_fixture"]
        prediction_page = _write_prediction_fixture(
            repo_root,
            content_root,
            data_root,
            sport,
            source_post,
            validation["prediction_references"],
        )

        override_config = tmp_root / "publication-smoke.toml"
        override_config.write_text(
            f'dataDir = "{data_root.as_posix()}"\n'
            f'staticDir = ["{static_root.as_posix()}"]\n',
            encoding="utf-8",
        )

        destination.mkdir(parents=True, exist_ok=True)
        command = [
            hugo,
            "--source",
            str(repo_root),
            "--config",
            f"{repo_root / 'config.toml'},{override_config}",
            "--contentDir",
            str(content_root),
            "--destination",
            str(destination),
            "--cleanDestinationDir",
            "--environment",
            "production",
        ]
        completed = subprocess.run(command, text=True, capture_output=True, check=False)
        if completed.returncode:
            raise RuntimeError(
                f"Targeted Hugo render failed for {sport} ({completed.returncode})\n"
                f"STDOUT:\n{completed.stdout}\nSTDERR:\n{completed.stderr}"
            )

    rendered: list[str] = []
    for _, output, marker in SPORT_PAGES.values():
        path = destination / sport / output
        if not path.is_file():
            raise FileNotFoundError(f"Hugo did not render required page: {path}")
        html = path.read_text(encoding="utf-8")
        if marker not in html:
            raise ValueError(f"Rendered page {path} is missing marker {marker!r}")
        rendered.append(str(path))

    prediction_output = (
        destination
        / "post"
        / "race-picks"
        / prediction_page.stem
        / "index.html"
    )
    if not prediction_output.is_file():
        raise FileNotFoundError(f"Hugo did not render prediction page: {prediction_output}")
    prediction_html = prediction_output.read_text(encoding="utf-8")
    if f"{sport} prediction publication smoke" not in prediction_html:
        raise ValueError(f"Prediction render is missing its smoke marker: {prediction_output}")
    rendered.append(str(prediction_output))

    return {
        "hugo_version": subprocess.run(
            [hugo, "version"], text=True, capture_output=True, check=True
        ).stdout.strip(),
        "rendered_pages": rendered,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--sport", required=True)
    parser.add_argument("--repo-root", type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument("--destination", type=Path)
    parser.add_argument("--report", type=Path)
    parser.add_argument("--validate-only", action="store_true")
    args = parser.parse_args()

    repo_root = args.repo_root.resolve()
    validation = validate_contract(repo_root, args.sport)
    report: dict[str, Any] = {"status": "passed", "validation": validation}
    if not args.validate_only:
        destination = (
            args.destination.resolve()
            if args.destination
            else repo_root / "artifacts" / "sport-publication" / args.sport
        )
        report["render"] = render_contract(repo_root, args.sport, validation, destination)

    if args.report:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
