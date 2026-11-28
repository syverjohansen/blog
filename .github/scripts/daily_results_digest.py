#!/usr/bin/env python3
"""Email result-run evidence and maintain durable athlete identity review issues."""

from __future__ import annotations

import argparse
import csv
from difflib import SequenceMatcher
import unicodedata
import html
import json
import os
from pathlib import Path
import subprocess

from fantasyxc_roster_digest import send_digest

REVIEW_LABEL = "athlete-identity"
NEW_METHODS = {
    "newcomer_fis_identity",
    "prediction_ledger_fis_identity",
    "new_official_ibu_identity",
    "accepted_fis_identity_review",
}


def review_key(sport, row):
    source_id = row.get("fis_code") or row.get("IBUId") or row.get("fis_competitor_id")
    if not source_id or row.get("sex") not in {"M", "L", "Mixed"}:
        raise ValueError(f"identity evidence lacks a stable sex/source ID: {row}")
    return f"{sport}/{row['sex']}/{source_id}"


def collect(roots):
    reviews, reports, matched = {}, [], set()
    for root in roots:
        if not root.exists():
            reports.append({"path": str(root), "status": "evidence_not_produced"})
            continue
        for path in sorted(root.rglob("*.json")):
            if not (path.name.endswith("report.json") or path.name == "report.json"):
                continue
            report = json.loads(path.read_text())
            reports.append(
                {
                    "path": str(path.relative_to(root)),
                    "status": report.get("status", "unknown"),
                }
            )
            for row in report.get("review_required", []):
                key = (row.get("sex"), row.get("fis_code") or row.get("IBUId"))
                reviews[key] = row
        for path in sorted(
            [*root.rglob("identity_map.csv"), *root.rglob("unresolved_identities.csv")]
        ):
            with path.open(newline="", encoding="utf-8-sig") as handle:
                for row in csv.DictReader(handle):
                    row["sex"] = (
                        row.get("sex")
                        or row.get("canonical_sex")
                        or row.get("event_sex", "")
                    )
                    if path.name == "unresolved_identities.csv":
                        row["status"] = "unresolved"
                    method = row.get("identity_match_method") or row.get("method", "")
                    key = (row.get("sex"), row.get("fis_code") or row.get("IBUId"))
                    if row.get("status") == "unresolved" or method in NEW_METHODS:
                        reviews.setdefault(
                            key, dict(row, reason=method or "unresolved")
                        )
                    elif method == "reviewed_alias":
                        matched.add(key)
    return list(reviews.values()), reports, matched


def accepted_provisionals(history_root, aliases):
    """Keep first-run newcomers visible even on subsequent no-change runs."""
    reviewed = set()
    if aliases and aliases.exists():
        with aliases.open(newline="", encoding="utf-8-sig") as handle:
            reviewed = {(r["sex"], r["fis_code"]) for r in csv.DictReader(handle)}
    found, historical = {}, {}
    nation_names = {}
    if history_root:
        nation_contract = (
            history_root.parent / "cross_country_world_cup_quotas_2027.json"
        )
        if nation_contract.is_file():
            nation_names = {
                r["code"]: r["name"]
                for r in json.loads(nation_contract.read_text())["nations"]
            }

    def nation(value):
        return nation_names.get(value.upper(), value).casefold()

    if history_root:
        for sex, filename in [("M", "men_chrono.csv"), ("L", "ladies_chrono.csv")]:
            path = history_root / filename
            if not path.exists():
                continue
            with path.open(newline="", encoding="utf-8-sig") as handle:
                for row in csv.DictReader(handle):
                    # The XC newcomer contract uses a seven-digit FIS code as
                    # provisional ID; legacy canonical IDs are separate integers.
                    code = row["ID"]
                    if len(code) < 7:
                        historical[(sex, code)] = (row["Skier"], row.get("Nation", ""))
                    if (
                        code.isdigit()
                        and len(code) == 7
                        and (sex, code) not in reviewed
                    ):
                        found[(sex, code)] = {
                            "sex": sex,
                            "fis_code": code,
                            "fis_name": row["Skier"],
                            "canonical_athlete_id": code,
                            "nation": row.get("Nation", ""),
                            "reason": "Accepted FIS-code newcomer has no reviewed historical alias",
                            "status": "provisional_newcomer",
                        }

    def comparable(name):
        return " ".join(
            sorted(
                unicodedata.normalize("NFKD", name)
                .encode("ascii", "ignore")
                .decode()
                .lower()
                .split()
            )
        )

    for (sex, _), row in found.items():
        row["suggestions"] = sorted(
            [
                {
                    "canonical_id": code,
                    "name": name,
                    "score": round(
                        SequenceMatcher(
                            None, comparable(row["fis_name"]), comparable(name)
                        ).ratio(),
                        3,
                    ),
                }
                for (history_sex, code), (name, country) in historical.items()
                if history_sex == sex and nation(country) == nation(row["nation"])
            ],
            key=lambda item: (-item["score"], item["canonical_id"]),
        )[:3]
        row["suggestions"] = [
            item for item in row["suggestions"] if item["score"] >= 0.65
        ]
    return list(found.values()), reviewed


def exclude_automatic_aliases(matched, aliases):
    if not aliases or not aliases.exists():
        return matched
    with aliases.open(newline="", encoding="utf-8-sig") as handle:
        automatic = {
            row["IBUId"]
            for row in csv.DictReader(handle)
            if row.get("reason") == "automatic official IBU debut identity"
        }
    return {
        (sex, source_id) for sex, source_id in matched if source_id not in automatic
    }


def api(endpoint, *, method="GET", payload=None):
    command = ["gh", "api", endpoint, "--method", method]
    if method == "GET":
        command += ["--paginate", "--slurp"]
    if payload is not None:
        command += ["--input", "-"]
    result = subprocess.run(
        command,
        input=json.dumps(payload) if payload is not None else None,
        capture_output=True,
        text=True,
        check=True,
    )
    return json.loads(result.stdout) if result.stdout.strip() else None


def sync_issues(repo, sport, reviews, matched, run_url):
    # Reuse existing email credentials; issues survive cache expiry and no-op runs.
    labels = [r for page in api(f"repos/{repo}/labels?per_page=100") for r in page]
    if not any(label["name"] == REVIEW_LABEL for label in labels):
        try:
            api(
                f"repos/{repo}/labels",
                method="POST",
                payload={
                    "name": REVIEW_LABEL,
                    "color": "D93F0B",
                    "description": "Athlete identities awaiting historical match or debut confirmation",
                },
            )
        except subprocess.CalledProcessError:
            labels = [
                r for page in api(f"repos/{repo}/labels?per_page=100") for r in page
            ]
            if not any(label["name"] == REVIEW_LABEL for label in labels):
                raise

    issues = [
        r
        for page in api(
            f"repos/{repo}/issues?state=all&labels={REVIEW_LABEL}&per_page=100"
        )
        for r in page
        if "pull_request" not in r
    ]
    indexed = {}
    for issue in issues:
        for line in (issue.get("body") or "").splitlines():
            if line.startswith("<!-- athlete-identity:"):
                indexed[
                    line.removeprefix("<!-- athlete-identity:").removesuffix(" -->")
                ] = issue
    for sex, code in matched:
        existing = indexed.get(f"{sport}/{sex}/{code}")
        if existing and existing["state"] == "open":
            api(
                f"repos/{repo}/issues/{existing['number']}",
                method="PATCH",
                payload={"state": "closed", "state_reason": "completed"},
            )
            existing["state"] = "closed"
    for row in reviews:
        key = review_key(sport, row)
        if key in indexed:
            continue  # A reviewed/closed debut stays reviewed on no-change runs.
        name = row.get("fis_name") or row.get("official_name") or "Unknown athlete"
        body = "\n".join(
            [
                f"<!-- athlete-identity:{key} -->",
                f"Observed name: **{name}**",
                f"Sport / sex / source ID: `{key}`",
                f"Reason: {row.get('reason', 'unmatched')}",
                f"Canonical ID used: `{row.get('canonical_athlete_id') or row.get('local_id') or 'none'}`",
                f"First run: {run_url}",
                "",
                "Suggested historical matches (review only; never applied automatically):",
                *[
                    f"- {item['name']} — ID {item['canonical_id']}, similarity {item['score']}"
                    for item in row.get("suggestions", [])
                ],
                "",
                "Resolve by adding a reviewed source-ID alias and replaying affected ratings, "
                "or confirm this is a real debut and close this issue. A name-only change does not repair Elo history.",
            ]
        )
        issue = api(
            f"repos/{repo}/issues",
            method="POST",
            payload={
                "title": f"[Identity review] {sport}: {name} ({key.rsplit('/', 1)[1]})",
                "body": body,
                "labels": [REVIEW_LABEL],
            },
        )
        indexed[key] = issue
    return [
        issue
        for key, issue in sorted(indexed.items())
        if key.startswith(sport + "/") and issue["state"] == "open"
    ]


def render(sport, as_of, status, run_url, reviews, reports, issues):
    lines = [
        f"Nordic Numbers daily results: {sport} through {as_of}",
        f"Run outcome: {status}",
        f"Run: {run_url}",
        "",
        f"Unmatched / automatic newcomers observed: {len(reviews)}",
    ]
    for row in reviews:
        name = row.get("fis_name") or row.get("official_name", "Unknown")
        source = row.get("fis_code") or row.get("IBUId") or row.get("fis_competitor_id")
        lines += [
            f"- {name} ({row.get('sex')}, {source}): {row.get('reason', 'unmatched')}"
        ]
        lines += [
            f"  Possible history: {item['name']} / ID {item['canonical_id']}"
            for item in row.get("suggestions", [])
        ]
    lines += ["", f"Open identity reviews: {len(issues)}"]
    lines += [f"- {issue['title']}: {issue['html_url']}" for issue in issues]
    lines += ["", "Stage evidence:"] + [
        f"- {row['path']}: {row['status']}" for row in reports
    ]
    if not reports:
        lines += [
            "No evidence was produced. Do not interpret this as zero unmatched athletes."
        ]
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sport", required=True)
    parser.add_argument("--as-of", required=True)
    parser.add_argument("--status", required=True)
    parser.add_argument("--root", action="append", type=Path, required=True)
    parser.add_argument("--history-root", type=Path)
    parser.add_argument("--aliases", type=Path)
    parser.add_argument("--ibu-aliases", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--send", action="store_true")
    parser.add_argument("--sync-issues", action="store_true")
    args = parser.parse_args()
    repo = os.environ["GITHUB_REPOSITORY"]
    run_url = f"https://github.com/{repo}/actions/runs/{os.environ['GITHUB_RUN_ID']}"
    reviews, reports, matched = collect(args.root)
    accepted, reviewed = accepted_provisionals(args.history_root, args.aliases)
    matched |= reviewed
    matched = exclude_automatic_aliases(matched, args.ibu_aliases)
    by_key = {review_key(args.sport, row): row for row in accepted + reviews}
    reviews = [
        row
        for key, row in sorted(by_key.items())
        if (row.get("sex"), row.get("fis_code")) not in reviewed
    ]
    issues, queue_error = [], None
    if args.sync_issues:
        try:
            issues = sync_issues(repo, args.sport, reviews, matched, run_url)
        except (subprocess.CalledProcessError, ValueError, KeyError) as error:
            queue_error = (
                f"Persistent identity queue update failed: {type(error).__name__}"
            )
            reports.append(
                {"path": "GitHub identity review queue", "status": queue_error}
            )
    text = render(
        args.sport, args.as_of, args.status, run_url, reviews, reports, issues
    )
    args.output.mkdir(parents=True, exist_ok=True)
    (args.output / "digest.txt").write_text(text)
    (args.output / "identity-review.json").write_text(
        json.dumps(
            {"reviews": reviews, "open_issues": issues, "reports": reports}, indent=2
        )
        + "\n"
    )
    with Path(os.environ.get("GITHUB_STEP_SUMMARY", args.output / "summary.md")).open(
        "a"
    ) as handle:
        handle.write(text)
    if args.send:
        address = os.environ["FANTASYXC_EMAIL_ADDRESS"]
        send_digest(
            sender=address,
            recipient=address,
            password=os.environ["FANTASYXC_SMTP_PASSWORD"],
            subject=f"Nordic Numbers {args.sport} results {args.as_of}: {args.status}; {len(issues)} identity reviews",
            text_body=text,
            html_body="<pre>" + html.escape(text) + "</pre>",
        )
    if queue_error:
        raise RuntimeError(queue_error)


if __name__ == "__main__":
    main()
