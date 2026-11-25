from pipelines.site_season_snapshot import (
    build_season_snapshot,
    current_season_rows,
)


def _row(
    *,
    athlete_id: str,
    season: int,
    race: int,
    event: str,
    date: str,
    pelo: int,
    elo: int,
) -> dict[str, str]:
    return {
        "Date": date,
        "City": "Example",
        "Country": "Example",
        "Sex": "L",
        "Distance": "10",
        "Event": event,
        "Place": "1",
        "Skier": f"Athlete {athlete_id}",
        "Nation": "Norway",
        "ID": athlete_id,
        "Season": str(season),
        "Race": str(race),
        "Pelo": str(pelo),
        "Elo": str(elo),
    }


def test_snapshot_compares_prior_close_to_latest_actual_current_rating() -> None:
    rows = [
        _row(
            athlete_id="1",
            season=2026,
            race=0,
            event="Offseason",
            date="2026-05-01",
            pelo=1500,
            elo=1470,
        ),
        _row(
            athlete_id="2",
            season=2026,
            race=0,
            event="Offseason",
            date="2026-05-01",
            pelo=1600,
            elo=1555,
        ),
        _row(
            athlete_id="1",
            season=2027,
            race=1,
            event="World Cup",
            date="2026-11-20",
            pelo=1470,
            elo=1480,
        ),
        _row(
            athlete_id="1",
            season=2027,
            race=2,
            event="World Cup",
            date="2026-11-21",
            pelo=1480,
            elo=1490,
        ),
        # Replay may carry a synthetic future boundary. It is not an actual race
        # and must never replace the latest current-season rating on the site.
        _row(
            athlete_id="1",
            season=2027,
            race=0,
            event="Offseason",
            date="2027-05-01",
            pelo=1490,
            elo=1461,
        ),
    ]

    latest = current_season_rows(rows, 2027)
    snapshot = build_season_snapshot(rows, 2027, {"Pelo": "Elo"})

    assert set(latest) == {"1"}
    assert latest["1"]["Race"] == "2"
    assert len(snapshot) == 1
    assert snapshot[0]["Date"] == "2027-05-01"
    assert snapshot[0]["Season"] == "2027"
    assert snapshot[0]["Race"] == "0"
    assert snapshot[0]["Pelo"] == "1500"
    assert snapshot[0]["Elo"] == "1490"
