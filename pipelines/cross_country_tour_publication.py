"""Build, render, and narrowly install a validated Tour de Ski candidate."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from datetime import date
from pathlib import Path
from typing import Any

from pipelines.cross_country_tour_shadow import (
    POINT_HEADERS,
    PROBABILITY_HEADERS,
    VALIDATION_SCHEMA,
)

CANDIDATE_SCHEMA = "nordicnumbers.cross-country-tour-candidate/v1"
RENDER_SCHEMA = "nordicnumbers.cross-country-tour-render/v1"
WORKBOOKS = {
    "men.xlsx": ("men.json", None, POINT_HEADERS),
    "ladies.xlsx": ("ladies.json", None, POINT_HEADERS),
    "men_position_probabilities.xlsx": (
        "men_position_probabilities.json",
        "Men Tour de Ski",
        PROBABILITY_HEADERS,
    ),
    "ladies_position_probabilities.xlsx": (
        "ladies_position_probabilities.json",
        "Ladies Tour de Ski",
        PROBABILITY_HEADERS,
    ),
}


class TourPublicationError(RuntimeError):
    """Validated Tour outputs cannot safely become a Hugo candidate."""


def _read_json(path: Path, label: str) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise TourPublicationError(f"cannot read {label}: {exc}") from exc
    if not isinstance(value, dict):
        raise TourPublicationError(f"{label} must be a JSON object")
    return value


def _json_bytes(value: Any) -> bytes:
    return (
        json.dumps(value, indent=2, sort_keys=True, allow_nan=False) + "\n"
    ).encode()


def _sha256(content: bytes) -> str:
    return hashlib.sha256(content).hexdigest()


def _atomic_write(path: Path, content: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(dir=path.parent, delete=False) as handle:
        handle.write(content)
        handle.flush()
        os.fsync(handle.fileno())
        temporary = Path(handle.name)
    os.replace(temporary, path)


def _validated_candidate_outputs(
    candidate_root: Path, logical_date: date
) -> dict[Path, bytes]:
    report = _read_json(
        candidate_root / "publication_candidate_report.json", "candidate report"
    )
    if report.get("schema_version") != CANDIDATE_SCHEMA:
        raise TourPublicationError("unsupported candidate report schema")
    if report.get("status") != "candidate":
        raise TourPublicationError("publication candidate is not ready")
    if report.get("logical_date") != logical_date.isoformat():
        raise TourPublicationError("candidate logical date does not match")
    if report.get("publication") != {"status": "disabled"}:
        raise TourPublicationError("candidate publication must remain disabled")
    outputs = report.get("outputs")
    if not isinstance(outputs, dict) or not outputs:
        raise TourPublicationError("candidate report has no outputs")
    documents: dict[Path, bytes] = {}
    for relative, metadata in outputs.items():
        path = Path(relative)
        if path.is_absolute() or ".." in path.parts:
            raise TourPublicationError(f"unsafe candidate output path: {relative}")
        try:
            content = (candidate_root / path).read_bytes()
        except OSError as exc:
            raise TourPublicationError(f"cannot read candidate output: {path}") from exc
        if not isinstance(metadata, dict) or metadata.get("sha256") != _sha256(content):
            raise TourPublicationError(f"candidate checksum mismatch: {relative}")
        documents[path] = content
    return documents


def build_tour_candidate(
    *,
    prediction_root: Path,
    validation_report_path: Path,
    logical_date: date,
    output_root: Path,
) -> dict[str, Any]:
    try:
        from openpyxl import load_workbook
    except ModuleNotFoundError as exc:
        raise TourPublicationError(
            "openpyxl is required to build the candidate"
        ) from exc

    validation = _read_json(validation_report_path, "validation report")
    logical_date_text = logical_date.isoformat()
    if validation.get("schema_version") != VALIDATION_SCHEMA:
        raise TourPublicationError("unsupported validation report schema")
    if validation.get("status") != "passed":
        raise TourPublicationError("Tour shadow validation did not pass")
    if validation.get("logical_date") != logical_date_text:
        raise TourPublicationError("validation logical date does not match")
    if validation.get("publication") != {"status": "disabled"}:
        raise TourPublicationError("validation publication must remain disabled")

    source_root = prediction_root.expanduser().resolve() / logical_date.strftime(
        "%Y%m%d"
    )
    validated_outputs = validation.get("outputs")
    if not isinstance(validated_outputs, dict):
        raise TourPublicationError("validation report has no outputs")
    date_key = logical_date.strftime("%Y%m%d")
    data_root = Path("data/cross-country/drafts/tds-picks") / date_key
    documents: dict[Path, bytes] = {}
    row_counts: dict[str, int] = {}

    for workbook_name, (
        json_name,
        expected_sheet,
        expected_headers,
    ) in WORKBOOKS.items():
        path = source_root / workbook_name
        try:
            content = path.read_bytes()
        except OSError as exc:
            raise TourPublicationError(
                f"cannot read validated workbook: {path}"
            ) from exc
        expected_sha = validated_outputs.get(workbook_name, {}).get("sha256")
        if expected_sha != _sha256(content):
            raise TourPublicationError(
                f"workbook checksum does not match validation: {workbook_name}"
            )
        try:
            workbook = load_workbook(path, read_only=True, data_only=True)
        except (OSError, ValueError) as exc:
            raise TourPublicationError(
                f"cannot open workbook: {workbook_name}"
            ) from exc
        if expected_sheet is None:
            if len(workbook.sheetnames) != 1:
                workbook.close()
                raise TourPublicationError(f"unexpected sheets in {workbook_name}")
            sheet = workbook.active
        else:
            if workbook.sheetnames != [expected_sheet]:
                workbook.close()
                raise TourPublicationError(f"unexpected sheets in {workbook_name}")
            sheet = workbook[expected_sheet]
        rows = list(sheet.iter_rows(values_only=True))
        workbook.close()
        if not rows or tuple(rows[0]) != expected_headers:
            raise TourPublicationError(f"unexpected headers in {workbook_name}")
        data_rows = [list(row) for row in rows[1:]]
        if not data_rows:
            raise TourPublicationError(f"validated workbook is empty: {workbook_name}")
        # Preserve the existing public table labels while the validated workbooks use
        # unambiguous machine-facing names such as Top-5.
        display_headers = [header.replace("-", " ") for header in expected_headers]
        document = {
            "sheet_name": sheet.title,
            "headers": display_headers,
            "rows": data_rows,
        }
        documents[data_root / json_name] = _json_bytes(document)
        row_counts[json_name] = len(data_rows)

    post = "\n".join(
        (
            "---",
            f'title: "Tour de Ski Picks for {logical_date.strftime("%m/%d/%Y")}"',
            f"date: {logical_date_text}T00:00:00+00:00",
            "draft: false",
            'tour_source_state: "validated-shadow"',
            'tags: ["predictions", "cross-country", "tour-de-ski"]',
            "---",
            "",
            "*See [Race Picks Methodology](/post/methods/race-picks/) for details.*",
            "",
            "> This candidate was generated from validated official Tour startlists. Publication remains disabled.",
            "",
            "## Men's Points",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/tds-picks/{date_key}/men" >}}}}',
            "",
            "## Ladies' Points",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/tds-picks/{date_key}/ladies" >}}}}',
            "",
            "## Men's Position Probabilities",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/tds-picks/{date_key}/men_position_probabilities" >}}}}',
            "",
            "## Ladies' Position Probabilities",
            "",
            f'{{{{< cross-country/datatable "cross-country/drafts/tds-picks/{date_key}/ladies_position_probabilities" >}}}}',
            "",
        )
    ).encode()
    documents[Path("content/post/tds-picks") / f"{date_key}.md"] = post

    output_root = output_root.expanduser().resolve()
    existing_report = output_root / "publication_candidate_report.json"
    if output_root.exists() and any(output_root.iterdir()):
        existing = _read_json(existing_report, "existing candidate report")
        if existing.get("schema_version") != CANDIDATE_SCHEMA:
            raise TourPublicationError(
                f"refusing to write into unrecognized output root: {output_root}"
            )
    output_metadata = {
        str(path): {
            "sha256": _sha256(content),
            **({"rows": row_counts[path.name]} if path.suffix == ".json" else {}),
        }
        for path, content in sorted(documents.items(), key=lambda item: str(item[0]))
    }
    report = {
        "schema_version": CANDIDATE_SCHEMA,
        "status": "candidate",
        "logical_date": logical_date_text,
        "source_validation_sha256": _sha256(validation_report_path.read_bytes()),
        "publication": {"status": "disabled"},
        "outputs": output_metadata,
    }
    for path, content in documents.items():
        _atomic_write(output_root / path, content)
    _atomic_write(existing_report, _json_bytes(report))
    return report


def render_tour_candidate(
    *,
    blog_root: Path,
    candidate_root: Path,
    logical_date: date,
    destination: Path,
    report_path: Path,
) -> dict[str, Any]:
    hugo = shutil.which("hugo")
    if not hugo:
        raise TourPublicationError("hugo is required to render the candidate")
    blog_root = blog_root.expanduser().resolve()
    candidate_root = candidate_root.expanduser().resolve()
    _validated_candidate_outputs(candidate_root, logical_date)
    destination = destination.expanduser().resolve()
    destination.parent.mkdir(parents=True, exist_ok=True)
    date_key = logical_date.strftime("%Y%m%d")
    with tempfile.TemporaryDirectory(prefix="xc-tour-hugo-") as directory:
        temporary = Path(directory)
        static_root = temporary / "static"
        static_root.mkdir()
        config = temporary / "hugo.toml"
        config.write_text(
            'baseURL = "https://test.invalid/"\n'
            'title = "Nordic Numbers Tour Shadow"\n'
            'theme = "white"\n'
            'languageCode = "en-us"\n'
            f'contentDir = "{(candidate_root / "content").as_posix()}"\n'
            f'dataDir = "{(candidate_root / "data").as_posix()}"\n'
            f'staticDir = "{static_root.as_posix()}"\n'
            'disableKinds = ["home", "section", "taxonomy", "term", "RSS", '
            '"sitemap", "robotsTXT", "404"]\n\n'
            "[params]\n"
            '  header_image = ""\n'
            '  description = "Deterministic Tour shadow"\n',
            encoding="utf-8",
        )
        completed = subprocess.run(
            (
                hugo,
                "--source",
                str(blog_root),
                "--config",
                str(config),
                "--destination",
                str(destination),
                "--cleanDestinationDir",
                "--noBuildLock",
                "--buildFuture",
                "--panicOnWarning",
                "--logLevel",
                "warn",
            ),
            cwd=blog_root,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )
    if completed.returncode != 0:
        raise TourPublicationError(
            "Tour Hugo render failed:\n" + completed.stdout.strip()
        )
    page_relative = Path("post/tds-picks") / date_key / "index.html"
    page = destination / page_relative
    if not page.is_file():
        raise TourPublicationError(f"expected rendered page is missing: {page}")
    html = page.read_text(encoding="utf-8")
    expected_markers = [
        f"Tour de Ski Picks for {logical_date.strftime('%m/%d/%Y')}",
        "table-cross-countrydraftstds-picks" + date_key + "men",
        "table-cross-countrydraftstds-picks"
        + date_key
        + "ladies_position_probabilities",
    ]
    for filename in ("men.json", "ladies.json"):
        document = _read_json(
            candidate_root
            / "data/cross-country/drafts/tds-picks"
            / date_key
            / filename,
            filename,
        )
        try:
            expected_markers.append(str(document["rows"][0][0]))
        except (KeyError, IndexError, TypeError) as exc:
            raise TourPublicationError(f"candidate data is empty: {filename}") from exc
    missing = [marker for marker in expected_markers if marker not in html]
    if missing:
        raise TourPublicationError(
            f"rendered candidate is missing expected markers: {missing}"
        )
    report = {
        "schema_version": RENDER_SCHEMA,
        "status": "passed",
        "logical_date": logical_date.isoformat(),
        "publication": {"status": "disabled"},
        "page": str(page_relative),
        "bytes": page.stat().st_size,
        "sha256": _sha256(page.read_bytes()),
    }
    _atomic_write(report_path.expanduser().resolve(), _json_bytes(report))
    return report


def install_tour_candidate(
    *,
    candidate_root: Path,
    render_report_path: Path,
    rendered_site_root: Path,
    logical_date: date,
    as_of: date,
    repository_root: Path,
) -> list[Path]:
    """Install only the exact candidate files proven by a successful render."""
    if as_of > logical_date:
        raise TourPublicationError("refusing to install a past Tour prediction")
    candidate_root = candidate_root.expanduser().resolve()
    documents = _validated_candidate_outputs(candidate_root, logical_date)
    date_key = logical_date.strftime("%Y%m%d")
    data_root = Path("data/cross-country/drafts/tds-picks") / date_key
    expected = {
        Path("content/post/tds-picks") / f"{date_key}.md",
        *(data_root / output[0] for output in WORKBOOKS.values()),
    }
    if set(documents) != expected:
        raise TourPublicationError(
            "candidate output set does not match publication contract"
        )

    render = _read_json(render_report_path, "render report")
    page_relative = Path("post/tds-picks") / date_key / "index.html"
    if (
        render.get("schema_version") != RENDER_SCHEMA
        or render.get("status") != "passed"
        or render.get("logical_date") != logical_date.isoformat()
        or render.get("publication") != {"status": "disabled"}
        or render.get("page") != str(page_relative)
    ):
        raise TourPublicationError("render report is not eligible for installation")
    page = rendered_site_root.expanduser().resolve() / page_relative
    try:
        page_content = page.read_bytes()
    except OSError as exc:
        raise TourPublicationError(f"cannot read rendered candidate: {page}") from exc
    if render.get("sha256") != _sha256(page_content):
        raise TourPublicationError("rendered candidate checksum does not match report")

    repository_root = repository_root.expanduser().resolve()
    if not (repository_root / ".git").exists():
        raise TourPublicationError("installation target is not a Git checkout")
    installed: list[Path] = []
    for relative, content in sorted(documents.items(), key=lambda item: str(item[0])):
        _atomic_write(repository_root / relative, content)
        installed.append(relative)
    return installed


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    build = subparsers.add_parser("build")
    build.add_argument("--prediction-root", required=True, type=Path)
    build.add_argument("--validation-report", required=True, type=Path)
    build.add_argument("--logical-date", required=True, type=date.fromisoformat)
    build.add_argument("--output-root", required=True, type=Path)
    render = subparsers.add_parser("render")
    render.add_argument("--blog-root", required=True, type=Path)
    render.add_argument("--candidate-root", required=True, type=Path)
    render.add_argument("--logical-date", required=True, type=date.fromisoformat)
    render.add_argument("--destination", required=True, type=Path)
    render.add_argument("--report", required=True, type=Path)
    install = subparsers.add_parser("install")
    install.add_argument("--candidate-root", required=True, type=Path)
    install.add_argument("--render-report", required=True, type=Path)
    install.add_argument("--rendered-site-root", required=True, type=Path)
    install.add_argument("--logical-date", required=True, type=date.fromisoformat)
    install.add_argument("--as-of", required=True, type=date.fromisoformat)
    install.add_argument("--repository-root", required=True, type=Path)
    args = parser.parse_args()
    if args.command == "build":
        report = build_tour_candidate(
            prediction_root=args.prediction_root,
            validation_report_path=args.validation_report,
            logical_date=args.logical_date,
            output_root=args.output_root,
        )
    elif args.command == "render":
        report = render_tour_candidate(
            blog_root=args.blog_root,
            candidate_root=args.candidate_root,
            logical_date=args.logical_date,
            destination=args.destination,
            report_path=args.report,
        )
    else:
        installed = install_tour_candidate(
            candidate_root=args.candidate_root,
            render_report_path=args.render_report,
            rendered_site_root=args.rendered_site_root,
            logical_date=args.logical_date,
            as_of=args.as_of,
            repository_root=args.repository_root,
        )
        report = {"installed": [str(path) for path in installed]}
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
