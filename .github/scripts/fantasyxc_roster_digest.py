#!/usr/bin/env python3
"""Render and email a deterministic FantasyXC roster digest."""

from __future__ import annotations

import argparse
import csv
import hashlib
import html
import json
import os
import smtplib
import ssl
from collections import defaultdict
from datetime import date, datetime
from email.message import EmailMessage
from pathlib import Path
from typing import Any
from urllib.parse import parse_qs, urlparse


def _race_id(startlist_url: str) -> str:
    return parse_qs(urlparse(startlist_url).query).get("raceid", [""])[0]


def _event_label(row: dict[str, str]) -> str:
    sex = {"L": "Women", "M": "Men"}.get(row.get("Sex", ""), row.get("Sex", ""))
    distance = row.get("Distance", "").strip()
    distance = "Sprint" if distance.lower() == "sprint" else f"{distance} km"
    technique = {"C": "classic", "F": "freestyle"}.get(
        row.get("Technique", ""), row.get("Technique", "").lower()
    )
    mass_start = " mass start" if row.get("MS") == "1" else ""
    return f"{sex} — {distance} {technique}{mass_start}".strip()


def load_events(schedule_path: Path, logical_date: date) -> tuple[dict[str, str], str]:
    events: dict[str, str] = {}
    venues: set[str] = set()
    with schedule_path.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            group_date = datetime.strptime(row["Date"], "%m/%d/%Y").date()
            if group_date != logical_date:
                continue
            race_id = _race_id(row["Startlist"])
            if race_id:
                events[race_id] = _event_label(row)
            if row.get("City"):
                venues.add(row["City"].strip())
    if not events:
        raise ValueError(f"no scheduled events found for {logical_date.isoformat()}")
    return events, ", ".join(sorted(venues))


def _entry_events(entry: dict[str, Any], events: dict[str, str]) -> list[str]:
    race_ids = entry.get("race_ids", [])
    if race_ids == ["*"] or "*" in race_ids:
        return ["Entire weekend"]
    return [events.get(str(race_id), f"FIS race {race_id}") for race_id in race_ids]


def render_digest(
    roster: dict[str, Any],
    events: dict[str, str],
    venue: str,
) -> tuple[str, str, dict[str, Any]]:
    grouped: dict[str, dict[str, dict[str, list[str]]]] = {
        "confirmed": defaultdict(lambda: defaultdict(list)),
        "excluded": defaultdict(lambda: defaultdict(list)),
    }
    for entry in roster.get("entries", []):
        status = entry["status"]
        nation = entry.get("canonical_nation") or "Unknown country"
        for event in _entry_events(entry, events):
            grouped[status][nation][event].append(entry["canonical_name"])

    logical_date = roster["logical_date"]
    unresolved = roster.get("unresolved", [])
    accepted_count = sum(
        len(names)
        for status in grouped.values()
        for country in status.values()
        for names in country.values()
    )
    subject = (
        f"FantasyXC roster: {venue or logical_date} — "
        f"{accepted_count} decisions, {len(unresolved)} unresolved"
    )

    text_lines = [
        subject,
        f"Weekend: {logical_date}",
        "Official FIS startlists override this announcement evidence.",
    ]
    html_parts = [
        "<html><body style='font-family:Arial,sans-serif;color:#1f2937'>",
        f"<h1>{html.escape(subject)}</h1>",
        f"<p><strong>Weekend:</strong> {html.escape(logical_date)}<br>",
        "Official FIS startlists override this announcement evidence.</p>",
    ]

    for status, heading in (("confirmed", "Racing"), ("excluded", "Not racing")):
        text_lines.extend(["", heading.upper()])
        html_parts.append(f"<h2>{heading}</h2>")
        if not grouped[status]:
            text_lines.append("None")
            html_parts.append("<p>None</p>")
            continue
        html_parts.append(
            "<table style='border-collapse:collapse;width:100%' border='1' "
            "cellpadding='6'><thead><tr><th>Country</th><th>Event</th>"
            "<th>Athletes</th></tr></thead><tbody>"
        )
        for nation in sorted(grouped[status]):
            text_lines.append(nation)
            for event in sorted(grouped[status][nation]):
                names = sorted(set(grouped[status][nation][event]))
                joined = ", ".join(names)
                text_lines.append(f"  {event}: {joined}")
                html_parts.append(
                    "<tr>"
                    f"<td>{html.escape(nation)}</td>"
                    f"<td>{html.escape(event)}</td>"
                    f"<td>{html.escape(joined)}</td>"
                    "</tr>"
                )
        html_parts.append("</tbody></table>")

    text_lines.extend(["", "STILL UP IN THE AIR (historical/quota fallback)"])
    html_parts.append("<h2>Still up in the air</h2>")
    html_parts.append(
        "<p>These rows were not applied. The existing historical/quota model "
        "will decide them unless an official FIS startlist becomes available.</p>"
    )
    if not unresolved:
        text_lines.append("None")
        html_parts.append("<p>None</p>")
    else:
        html_parts.append(
            "<table style='border-collapse:collapse;width:100%' border='1' "
            "cellpadding='6'><thead><tr><th>Country</th><th>Event</th>"
            "<th>Athlete/evidence</th><th>Reason</th></tr></thead><tbody>"
        )
        for row in sorted(
            unresolved,
            key=lambda item: (
                item.get("canonical_nation", ""),
                item.get("canonical_name", ""),
                item.get("evidence_text", ""),
            ),
        ):
            nation = row.get("canonical_nation") or "Unknown country"
            labels = _entry_events(row, events) if row.get("race_ids") else ["Unclear"]
            event = ", ".join(labels)
            evidence = row.get("canonical_name") or row.get("evidence_text") or "Unknown"
            reason = row.get("reason", "Requires review")
            text_lines.append(f"{nation} | {event} | {evidence} | {reason}")
            source_url = row.get("source_url", "")
            evidence_html = html.escape(evidence)
            if source_url.startswith("https://"):
                evidence_html = (
                    f"<a href='{html.escape(source_url, quote=True)}'>{evidence_html}</a>"
                )
            html_parts.append(
                "<tr>"
                f"<td>{html.escape(nation)}</td>"
                f"<td>{html.escape(event)}</td>"
                f"<td>{evidence_html}</td>"
                f"<td>{html.escape(reason)}</td>"
                "</tr>"
            )
        html_parts.append("</tbody></table>")

    html_parts.append("</body></html>")
    canonical = json.dumps(roster, sort_keys=True, separators=(",", ":")).encode()
    metadata = {
        "subject": subject,
        "logical_date": logical_date,
        "venue": venue,
        "accepted_decisions": accepted_count,
        "unresolved": len(unresolved),
        "digest_sha256": hashlib.sha256(canonical).hexdigest(),
    }
    return "\n".join(text_lines) + "\n", "".join(html_parts), metadata


def send_digest(
    *,
    sender: str,
    recipient: str,
    password: str,
    subject: str,
    text_body: str,
    html_body: str,
    host: str = "smtp.gmail.com",
    port: int = 465,
) -> None:
    if not sender or not recipient or not password:
        raise ValueError("sender, recipient, and SMTP password are required")
    message = EmailMessage()
    message["From"] = sender
    message["To"] = recipient
    message["Subject"] = subject
    message.set_content(text_body)
    message.add_alternative(html_body, subtype="html")
    context = ssl.create_default_context()
    with smtplib.SMTP_SSL(host, port, context=context, timeout=30) as smtp:
        smtp.login(sender, password)
        smtp.send_message(message)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)
    render = subparsers.add_parser("render")
    render.add_argument("--roster", required=True, type=Path)
    render.add_argument("--schedule", required=True, type=Path)
    render.add_argument("--logical-date", required=True, type=date.fromisoformat)
    render.add_argument("--text-output", required=True, type=Path)
    render.add_argument("--html-output", required=True, type=Path)
    render.add_argument("--metadata-output", required=True, type=Path)
    send = subparsers.add_parser("send")
    send.add_argument("--metadata", required=True, type=Path)
    send.add_argument("--text", required=True, type=Path)
    send.add_argument("--html", required=True, type=Path)
    send.add_argument("--sender", required=True)
    send.add_argument("--recipient", required=True)
    send.add_argument("--password-env", default="FANTASYXC_SMTP_PASSWORD")
    args = parser.parse_args()

    if args.command == "render":
        roster = json.loads(args.roster.read_text(encoding="utf-8"))
        if roster.get("logical_date") != args.logical_date.isoformat():
            raise ValueError("roster logical date does not match requested digest date")
        events, venue = load_events(args.schedule, args.logical_date)
        text_body, html_body, metadata = render_digest(roster, events, venue)
        for path in (args.text_output, args.html_output, args.metadata_output):
            path.parent.mkdir(parents=True, exist_ok=True)
        args.text_output.write_text(text_body, encoding="utf-8")
        args.html_output.write_text(html_body, encoding="utf-8")
        args.metadata_output.write_text(
            json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        print(json.dumps(metadata, sort_keys=True))
        return 0

    metadata = json.loads(args.metadata.read_text(encoding="utf-8"))
    send_digest(
        sender=args.sender,
        recipient=args.recipient,
        password=os.environ.get(args.password_env, ""),
        subject=metadata["subject"],
        text_body=args.text.read_text(encoding="utf-8"),
        html_body=args.html.read_text(encoding="utf-8"),
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
