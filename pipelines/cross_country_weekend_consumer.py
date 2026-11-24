"""Validate a ski weekend bundle and materialize isolated model inputs.

This consumer does not run the model or publish. It verifies the checksummed
``nordicnumbers.cross-country-weekend-inputs/v1`` boundary and translates the
event-level athlete rows into the per-gender participation matrix expected by
the existing weekly prediction model.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import os
import shutil
import tempfile
from collections import defaultdict
from datetime import date
from pathlib import Path
from typing import Any
from urllib.parse import parse_qs, urlparse

BUNDLE_SCHEMA = "nordicnumbers.cross-country-weekend-inputs/v1"
RUNTIME_SCHEMA = "nordicnumbers.cross-country-weekend-runtime/v1"
SCHEDULE_FIELDS = (
    "Date",
    "Race_Date",
    "Startlist",
    "Sex",
    "City",
    "Country",
    "Distance",
    "Technique",
    "MS",
    "Period",
    "Pursuit",
    "Stage",
    "Final_Climb",
    "Elevation",
    "Race",
    "Championship",
)
STARTLIST_FIELDS = (
    "group_date",
    "race_date",
    "race_id",
    "sex",
    "city",
    "country",
    "distance",
    "technique",
    "canonical_athlete_id",
    "canonical_name",
    "canonical_nation",
    "fis_competitor_id",
    "fis_code",
    "identity_match_method",
    "selection_status",
    "source_mode",
    "source_url",
    "source_sha256",
)
QUOTA_SOURCE_FIELDS = (
    "quota_contract_version",
    "base_nation_quota",
    "shared_event_quota",
    "personal_quota",
    "personal_start_right",
    "is_host_nation",
    "team_event_weekend_exception",
    "max_nation_starters",
)
QUOTA_RUNTIME_FIELDS = (
    "Quota_Contract_Version",
    "Base_Nation_Quota",
    "Shared_Event_Quota",
    "Personal_Quota",
    "Personal_Start_Right",
    "Is_Host_Nation",
    "Team_Event_Weekend_Exception",
    "Max_Nation_Starters",
)
ELO_STREAMS = (
    "Elo",
    "Distance_Elo",
    "Distance_C_Elo",
    "Distance_F_Elo",
    "Sprint_Elo",
    "Sprint_C_Elo",
    "Sprint_F_Elo",
    "Classic_Elo",
    "Freestyle_Elo",
)
SHADOW_RATING_FIELDS = (
    *STARTLIST_FIELDS,
    "rating_source",
    "rating_as_of_date",
    "rating_k_factor",
    *ELO_STREAMS,
)
SHADOW_RATING_SCHEMA = "nordicnumbers.cross-country-prediction-elo-shadow/v1"


class WeekendConsumerError(RuntimeError):
    """A weekend bundle cannot be safely passed to the prediction model."""


def _sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _read_csv(path: Path) -> tuple[tuple[str, ...], list[dict[str, str]]]:
    try:
        with path.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            return tuple(reader.fieldnames or ()), [dict(row) for row in reader]
    except OSError as exc:
        raise WeekendConsumerError(f"cannot read bundle file {path}: {exc}") from exc


def _race_id(url: str) -> str:
    race_ids = parse_qs(urlparse(url).query).get("raceid", [])
    if len(race_ids) != 1 or not race_ids[0].isdigit():
        raise WeekendConsumerError(f"invalid schedule startlist URL: {url!r}")
    return race_ids[0]


def _load_report(bundle_dir: Path) -> dict[str, Any]:
    path = bundle_dir / "report.json"
    try:
        report = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise WeekendConsumerError(f"cannot read bundle report: {exc}") from exc
    if report.get("schema_version") != BUNDLE_SCHEMA:
        raise WeekendConsumerError("unsupported weekend bundle schema")
    if report.get("status") != "candidate":
        raise WeekendConsumerError(
            f"weekend bundle is not a candidate: {report.get('status')!r}"
        )
    if report.get("publication") != {"status": "disabled"}:
        raise WeekendConsumerError("source bundle must keep publication disabled")
    return report


def _verify_outputs(bundle_dir: Path, report: dict[str, Any]) -> None:
    outputs = report.get("outputs")
    if not isinstance(outputs, dict):
        raise WeekendConsumerError("bundle report has no output checksums")
    expected_names = {"schedule.csv", "men_startlist.csv", "ladies_startlist.csv"}
    if set(outputs) != expected_names:
        raise WeekendConsumerError("bundle report output set is incomplete")
    for name in sorted(expected_names):
        metadata = outputs[name]
        path = bundle_dir / name
        try:
            content = path.read_bytes()
        except OSError as exc:
            raise WeekendConsumerError(
                f"cannot read bundle output {name}: {exc}"
            ) from exc
        if not isinstance(metadata, dict) or metadata.get("sha256") != _sha256(content):
            raise WeekendConsumerError(f"bundle checksum mismatch: {name}")
        observed_rows = max(0, content.count(b"\n") - 1)
        if metadata.get("rows") != observed_rows:
            raise WeekendConsumerError(f"bundle row count mismatch: {name}")


def _validate_schedule(
    bundle_dir: Path, logical_date: date
) -> tuple[list[dict[str, str]], dict[tuple[str, str], int]]:
    fields, rows = _read_csv(bundle_dir / "schedule.csv")
    if fields != SCHEDULE_FIELDS:
        raise WeekendConsumerError(f"unexpected schedule fields: {fields}")
    if not rows:
        raise WeekendConsumerError("candidate bundle has no normal-weekend races")
    event_order: dict[tuple[str, str], int] = {}
    sex_counts: dict[str, int] = defaultdict(int)
    for row in rows:
        if row["Date"] != logical_date.strftime("%m/%d/%Y"):
            raise WeekendConsumerError(
                "schedule group date does not match logical date"
            )
        sex = row["Sex"]
        if sex not in {"M", "L"}:
            raise WeekendConsumerError(f"unsupported schedule sex: {sex!r}")
        if row["Distance"] in {"Rel", "Ts"} or row["Stage"] == "1":
            raise WeekendConsumerError("specialized race entered normal-weekend bundle")
        race_id = _race_id(row["Startlist"])
        key = (sex, race_id)
        if key in event_order:
            raise WeekendConsumerError(f"duplicate schedule race: {sex}/{race_id}")
        sex_counts[sex] += 1
        event_order[key] = sex_counts[sex]
    return rows, event_order


def _validate_startlists(
    bundle_dir: Path,
    logical_date: date,
    event_order: dict[tuple[str, str], int],
) -> tuple[
    dict[str, list[dict[str, str]]], dict[tuple[str, str], str], bool
]:
    by_sex: dict[str, list[dict[str, str]]] = {}
    event_modes: dict[tuple[str, str], set[str]] = defaultdict(set)
    seen: set[tuple[str, str, str]] = set()
    quota_groups: dict[tuple[str, str, str], tuple[str, ...]] = {}
    quota_contract_enabled: bool | None = None
    for sex, filename in (("M", "men_startlist.csv"), ("L", "ladies_startlist.csv")):
        fields, rows = _read_csv(bundle_dir / filename)
        has_quota_contract = fields == (*STARTLIST_FIELDS, *QUOTA_SOURCE_FIELDS)
        if fields != STARTLIST_FIELDS and not has_quota_contract:
            raise WeekendConsumerError(f"unexpected startlist fields: {filename}")
        if quota_contract_enabled is None:
            quota_contract_enabled = has_quota_contract
        elif quota_contract_enabled != has_quota_contract:
            raise WeekendConsumerError("quota contract is missing from one startlist")
        by_sex[sex] = rows
        for row in rows:
            race_id = row["race_id"]
            key = (sex, race_id)
            athlete_id = row["canonical_athlete_id"].strip()
            if row["sex"] != sex or key not in event_order:
                raise WeekendConsumerError(
                    f"startlist row does not match schedule: {sex}/{race_id}"
                )
            if row["group_date"] != logical_date.isoformat():
                raise WeekendConsumerError(
                    "startlist group date does not match logical date"
                )
            if not athlete_id or not row["canonical_name"].strip():
                raise WeekendConsumerError(
                    "startlist row has incomplete canonical identity"
                )
            identity = (sex, race_id, athlete_id)
            if identity in seen:
                raise WeekendConsumerError(f"duplicate startlist athlete: {identity}")
            seen.add(identity)
            if has_quota_contract:
                try:
                    base = int(row["base_nation_quota"])
                    shared = int(row["shared_event_quota"])
                    personal = int(row["personal_quota"])
                    host = int(row["is_host_nation"])
                    team = int(row["team_event_weekend_exception"])
                    maximum = int(row["max_nation_starters"])
                except ValueError as exc:
                    raise WeekendConsumerError(
                        f"invalid quota assignment: {sex}/{race_id}/{athlete_id}"
                    ) from exc
                if (
                    not row["quota_contract_version"].strip()
                    or base not in {2, 3, 4, 5, 6}
                    or shared < base
                    or shared > maximum
                    or personal not in {0, 1}
                    or host not in {0, 1}
                    or team not in {0, 1}
                    or maximum != 10
                    or bool(row["personal_start_right"].strip()) != bool(personal)
                ):
                    raise WeekendConsumerError(
                        f"invalid quota assignment: {sex}/{race_id}/{athlete_id}"
                    )
                quota_group = (sex, race_id, row["canonical_nation"])
                shared_assignment = tuple(
                    row[field]
                    for field in (
                        "quota_contract_version",
                        "base_nation_quota",
                        "shared_event_quota",
                        "is_host_nation",
                        "team_event_weekend_exception",
                        "max_nation_starters",
                    )
                )
                previous_assignment = quota_groups.setdefault(
                    quota_group, shared_assignment
                )
                if previous_assignment != shared_assignment:
                    raise WeekendConsumerError(
                        f"inconsistent nation quota assignment: {quota_group}"
                    )
            mode = row["source_mode"]
            status = row["selection_status"]
            if mode == "official_fis_startlist":
                if status != "confirmed" or not row["fis_competitor_id"].strip():
                    raise WeekendConsumerError(
                        "official startlist row is not confirmed"
                    )
            elif mode == "latest_season_fallback":
                if status != "candidate_pool" or row["fis_competitor_id"].strip():
                    raise WeekendConsumerError("fallback startlist row is mislabelled")
            else:
                raise WeekendConsumerError(
                    f"unsupported startlist source mode: {mode!r}"
                )
            event_modes[key].add(mode)
    missing_events = sorted(set(event_order) - set(event_modes))
    if missing_events:
        raise WeekendConsumerError(
            f"startlist rows missing for events: {missing_events}"
        )
    mixed = sorted(key for key, modes in event_modes.items() if len(modes) != 1)
    if mixed:
        raise WeekendConsumerError(f"event mixes startlist source modes: {mixed}")
    return (
        by_sex,
        {key: next(iter(modes)) for key, modes in event_modes.items()},
        bool(quota_contract_enabled),
    )


def _load_shadow_ratings(
    root: Path,
    by_sex: dict[str, list[dict[str, str]]],
) -> dict[tuple[str, str, str], dict[str, str]]:
    ratings: dict[tuple[str, str, str], dict[str, str]] = {}
    for sex, gender in (("M", "men"), ("L", "ladies")):
        source_rows = by_sex[sex]
        if not source_rows:
            continue
        directory = root / gender
        report_path = directory / "report.json"
        csv_path = directory / "startlist_prediction_elo_shadow.csv"
        try:
            report = json.loads(report_path.read_text(encoding="utf-8"))
            payload = csv_path.read_bytes()
        except (OSError, json.JSONDecodeError) as exc:
            raise WeekendConsumerError(
                f"cannot read {gender} shadow rating overlay: {exc}"
            ) from exc
        if report.get("schema_version") != SHADOW_RATING_SCHEMA:
            raise WeekendConsumerError("unsupported shadow rating schema")
        if report.get("publication") != {"status": "disabled"}:
            raise WeekendConsumerError("shadow ratings must keep publication disabled")
        if report.get("selected_k_factor") != 4.0:
            raise WeekendConsumerError("shadow ratings must use calibrated K=4")
        if report.get("output_sha256") != _sha256(payload):
            raise WeekendConsumerError("shadow rating checksum mismatch")
        fields, overlay_rows = _read_csv(csv_path)
        if fields != SHADOW_RATING_FIELDS:
            raise WeekendConsumerError("unexpected shadow rating fields")

        expected = {
            (row["race_id"], row["canonical_athlete_id"]): row for row in source_rows
        }
        observed: set[tuple[str, str]] = set()
        for line, row in enumerate(overlay_rows, start=2):
            key = (row["race_id"], row["canonical_athlete_id"])
            if key in observed or key not in expected:
                raise WeekendConsumerError(
                    f"{csv_path}:{line}: unexpected or duplicate shadow athlete"
                )
            observed.add(key)
            if any(row[field] != expected[key][field] for field in STARTLIST_FIELDS):
                raise WeekendConsumerError(
                    f"{csv_path}:{line}: shadow identity differs from bundle"
                )
            athlete_id = row["canonical_athlete_id"]
            source = row["rating_source"]
            allowed = (
                {"fixed_anchor_prediction_elo", "newcomer_prior"}
                if athlete_id.startswith("fis:")
                else {"accepted_official_elo"}
            )
            if source not in allowed:
                raise WeekendConsumerError(
                    f"{csv_path}:{line}: rating source is incompatible with identity"
                )
            if source == "accepted_official_elo":
                if row["rating_k_factor"]:
                    raise WeekendConsumerError(
                        f"{csv_path}:{line}: official Elo cannot have a K factor"
                    )
            elif row["rating_k_factor"] != "4.0":
                raise WeekendConsumerError(
                    f"{csv_path}:{line}: private Elo must use K=4"
                )
            as_of = row["rating_as_of_date"]
            if as_of:
                try:
                    observed_date = date.fromisoformat(as_of)
                    race_date = date.fromisoformat(row["race_date"])
                except ValueError as exc:
                    raise WeekendConsumerError(
                        f"{csv_path}:{line}: invalid shadow rating date"
                    ) from exc
                if observed_date >= race_date:
                    raise WeekendConsumerError(
                        f"{csv_path}:{line}: shadow rating is not strictly pre-race"
                    )
            try:
                for stream in ELO_STREAMS:
                    float(row[stream])
            except ValueError as exc:
                raise WeekendConsumerError(
                    f"{csv_path}:{line}: invalid shadow rating"
                ) from exc
            ratings[(sex, *key)] = row
        if observed != set(expected):
            raise WeekendConsumerError(
                f"{gender} shadow rating overlay does not cover the bundle"
            )
    return ratings


def _runtime_rows(
    sex: str,
    source_rows: list[dict[str, str]],
    event_order: dict[tuple[str, str], int],
    event_modes: dict[tuple[str, str], str],
    shadow_ratings: dict[tuple[str, str, str], dict[str, str]] | None = None,
    quota_contract_enabled: bool = False,
) -> tuple[list[str], list[dict[str, str]]]:
    race_ids = [
        race_id
        for (event_sex, race_id), _ in sorted(
            event_order.items(), key=lambda item: item[1]
        )
        if event_sex == sex
    ]
    athletes: dict[str, dict[str, str]] = {}
    present: set[tuple[str, str]] = set()
    for row in source_rows:
        athlete_id = row["canonical_athlete_id"]
        identity = {
            "ID": athlete_id,
            "FIS_Code": row["fis_code"],
            "Skier": row["canonical_name"],
            "Nation": row["canonical_nation"],
        }
        if quota_contract_enabled:
            identity.update(
                dict(
                    zip(
                        QUOTA_RUNTIME_FIELDS,
                        (row[field] for field in QUOTA_SOURCE_FIELDS),
                        strict=True,
                    )
                )
            )
        if shadow_ratings is not None:
            rating = shadow_ratings[(sex, row["race_id"], athlete_id)]
            identity.update(
                {
                    "Shadow_Rating_Source": rating["rating_source"],
                    "Shadow_Rating_As_Of_Date": rating["rating_as_of_date"],
                    "Shadow_Rating_K_Factor": rating["rating_k_factor"],
                    **{stream: rating[stream] for stream in ELO_STREAMS},
                }
            )
        previous = athletes.get(athlete_id)
        if previous:
            if any(
                previous[field] != identity[field]
                for field in (
                    "ID",
                    "Skier",
                    "Nation",
                    *(
                        (
                            "Shadow_Rating_Source",
                            "Shadow_Rating_As_Of_Date",
                            "Shadow_Rating_K_Factor",
                            *ELO_STREAMS,
                        )
                        if shadow_ratings is not None
                        else ()
                    ),
                    *(QUOTA_RUNTIME_FIELDS if quota_contract_enabled else ()),
                )
            ):
                raise WeekendConsumerError(
                    f"canonical identity changes inside bundle: {sex}/{athlete_id}"
                )
            if (
                previous["FIS_Code"]
                and identity["FIS_Code"]
                and previous["FIS_Code"] != identity["FIS_Code"]
            ):
                raise WeekendConsumerError(
                    f"FIS code changes inside bundle: {sex}/{athlete_id}"
                )
            identity["FIS_Code"] = previous["FIS_Code"] or identity["FIS_Code"]
        athletes[athlete_id] = identity
        present.add((row["race_id"], athlete_id))

    fields = ["ID", "FIS_Code", "Skier", "Nation"]
    if quota_contract_enabled:
        fields.extend(QUOTA_RUNTIME_FIELDS)
    if shadow_ratings is not None:
        fields.extend(
            (
                "Shadow_Rating_Source",
                "Shadow_Rating_As_Of_Date",
                "Shadow_Rating_K_Factor",
                *ELO_STREAMS,
            )
        )
    for index in range(1, len(race_ids) + 1):
        fields.extend((f"Race{index}_Prob", f"Race{index}_Official"))
    rows: list[dict[str, str]] = []
    for athlete_id in sorted(athletes, key=lambda value: (not value.isdigit(), value)):
        row = dict(athletes[athlete_id])
        for index, race_id in enumerate(race_ids, start=1):
            official = event_modes[(sex, race_id)] == "official_fis_startlist"
            row[f"Race{index}_Prob"] = (
                ("1" if (race_id, athlete_id) in present else "0") if official else ""
            )
            row[f"Race{index}_Official"] = "true" if official else "false"
        rows.append(row)
    return fields, rows


def _csv_bytes(
    fields: list[str] | tuple[str, ...], rows: list[dict[str, str]]
) -> bytes:
    output = io.StringIO(newline="")
    writer = csv.DictWriter(output, fieldnames=fields, lineterminator="\n")
    writer.writeheader()
    writer.writerows(rows)
    return output.getvalue().encode()


def materialize_weekend_runtime(
    *,
    bundle_dir: Path,
    logical_date: date,
    output_dir: Path,
    prediction_elo_root: Path | None = None,
) -> dict[str, Any]:
    bundle_dir = bundle_dir.expanduser().resolve()
    output_dir = output_dir.expanduser().resolve()
    if output_dir == bundle_dir or bundle_dir in output_dir.parents:
        raise WeekendConsumerError("runtime output cannot be inside the source bundle")
    report = _load_report(bundle_dir)
    if report.get("logical_date") != logical_date.isoformat():
        raise WeekendConsumerError(
            "bundle report logical date does not match invocation"
        )
    _verify_outputs(bundle_dir, report)
    schedule_rows, event_order = _validate_schedule(bundle_dir, logical_date)
    by_sex, event_modes, quota_contract_enabled = _validate_startlists(
        bundle_dir, logical_date, event_order
    )
    quota_version = None
    if quota_contract_enabled:
        quota_versions = {
            row["quota_contract_version"]
            for rows in by_sex.values()
            for row in rows
        }
        if len(quota_versions) != 1:
            raise WeekendConsumerError("bundle contains multiple quota contracts")
        quota_version = next(iter(quota_versions))
        quota_report = report.get("quota_contract")
        if (
            not isinstance(quota_report, dict)
            or quota_report.get("version") != quota_version
            or not quota_report.get("sha256")
        ):
            raise WeekendConsumerError("bundle quota metadata does not match rows")
    shadow_ratings = None
    if prediction_elo_root is not None:
        prediction_elo_root = prediction_elo_root.expanduser().resolve()
        if (
            prediction_elo_root == bundle_dir
            or bundle_dir in prediction_elo_root.parents
        ):
            raise WeekendConsumerError(
                "shadow rating input cannot be inside the bundle"
            )
        shadow_ratings = _load_shadow_ratings(prediction_elo_root, by_sex)

    documents: dict[str, bytes] = {
        "weekends.csv": _csv_bytes(SCHEDULE_FIELDS, schedule_rows),
    }
    row_counts: dict[str, int] = {"weekends.csv": len(schedule_rows)}
    for sex, gender in (("M", "men"), ("L", "ladies")):
        fields, rows = _runtime_rows(
            sex,
            by_sex[sex],
            event_order,
            event_modes,
            shadow_ratings,
            quota_contract_enabled,
        )
        name = f"startlist_weekend_{gender}.csv"
        documents[name] = _csv_bytes(fields, rows)
        row_counts[name] = len(rows)

    runtime_report: dict[str, Any] = {
        "schema_version": RUNTIME_SCHEMA,
        "status": "ready_for_shadow_simulation",
        "logical_date": logical_date.isoformat(),
        "source_bundle_schema": BUNDLE_SCHEMA,
        "source_report_sha256": _sha256((bundle_dir / "report.json").read_bytes()),
        "prediction_elo_shadow": {
            "enabled": shadow_ratings is not None,
            "selected_k_factor": 4.0 if shadow_ratings is not None else None,
        },
        "quota_contract": {
            "enabled": quota_contract_enabled,
            "version": quota_version,
        },
        "event_modes": {
            f"{sex}/{race_id}": event_modes[(sex, race_id)]
            for sex, race_id in sorted(event_modes)
        },
        "publication": {"status": "disabled"},
        "outputs": {
            name: {"sha256": _sha256(content), "rows": row_counts[name]}
            for name, content in sorted(documents.items())
        },
    }
    documents["runtime_report.json"] = (
        json.dumps(runtime_report, indent=2, sort_keys=True) + "\n"
    ).encode()

    output_dir.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(
        tempfile.mkdtemp(prefix=f".{output_dir.name}.", dir=output_dir.parent)
    )
    try:
        for name, content in documents.items():
            (staging / name).write_bytes(content)
        if output_dir.exists():
            marker = output_dir / "runtime_report.json"
            try:
                existing = json.loads(marker.read_text(encoding="utf-8"))
            except (OSError, json.JSONDecodeError) as exc:
                raise WeekendConsumerError(
                    f"refusing to replace unrecognized output directory: {output_dir}"
                ) from exc
            if existing.get("schema_version") != RUNTIME_SCHEMA:
                raise WeekendConsumerError(
                    f"refusing to replace unrecognized output directory: {output_dir}"
                )
            shutil.rmtree(output_dir)
        os.replace(staging, output_dir)
    except Exception:
        shutil.rmtree(staging, ignore_errors=True)
        raise
    return runtime_report


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--bundle-dir", required=True, type=Path)
    parser.add_argument("--logical-date", required=True, type=date.fromisoformat)
    parser.add_argument("--output-dir", required=True, type=Path)
    parser.add_argument("--prediction-elo-root", type=Path)
    args = parser.parse_args()
    report = materialize_weekend_runtime(
        bundle_dir=args.bundle_dir,
        logical_date=args.logical_date,
        output_dir=args.output_dir,
        prediction_elo_root=args.prediction_elo_root,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
