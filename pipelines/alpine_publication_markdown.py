"""Render the canonical Hugo page for validated Alpine race predictions."""

from __future__ import annotations

from datetime import date


def publication_markdown(logical_date: date, genders: list[str]) -> bytes:
    labels = {"men": "Men", "ladies": "Ladies"}
    date_key = logical_date.strftime("%Y%m%d")
    lines = [
        "---",
        f'title: "Race Picks for {logical_date.strftime("%m/%d/%Y")}"',
        f"date: {logical_date.isoformat()}T00:00:00+00:00",
        "draft: false",
        'tags: ["predictions", "skiing", "race-picks"]',
        "---",
        "",
        (
            "*See [Race Picks Methodology](/post/alpine/methods/race-picks/) for "
            "details on how these predictions are generated.*"
        ),
        "",
        "## Alpine",
    ]
    for gender in genders:
        lines.extend(
            [
                "",
                f"### {labels[gender]}",
                "",
                "#### Position Probabilities",
                "",
                (
                    "{{< alpine/datatable "
                    f'"alpine/drafts/race-picks/{date_key}/'
                    f'{gender}_position_probabilities" >}}}}'
                ),
            ]
        )
    return ("\n".join(lines) + "\n").encode()
