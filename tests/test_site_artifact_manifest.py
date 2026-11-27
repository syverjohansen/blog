from __future__ import annotations

from fnmatch import fnmatchcase
import json
import re
import unittest
from pathlib import Path


BLOG_ROOT = Path(__file__).resolve().parents[1]
MANIFEST_PATH = BLOG_ROOT / "pipelines/site_artifacts.json"
LAYOUT_ROOT = BLOG_ROOT / "layouts"
SPORTS = {"alpine", "cross-country", "biathlon", "nordic-combined", "skijump"}
REFERENCE = re.compile(
    r"/?(python/(?P<sport>alpine|cross-country|biathlon|nordic-combined|skijump)"
    r"/excel365/(?P<relative>[A-Za-z0-9_./${}-]+\.json))"
)
BASE_ASSIGNMENT = re.compile(
    r"(?:const|let|var)\s+(?P<variable>\w*[Bb]asePath)\s*=\s*[^;\n]*"
    r"python/(?P<sport>alpine|cross-country|biathlon|nordic-combined|skijump)"
    r"/excel365/"
)
TEMPLATE_VALUE = re.compile(r"\$\{[^}]+\}")


def layout_references() -> list[tuple[Path, str, str, str]]:
    references: set[tuple[Path, str, str, str]] = set()
    for layout in sorted(path for path in LAYOUT_ROOT.rglob("*") if path.is_file()):
        source = layout.read_text(encoding="utf-8")
        for match in REFERENCE.finditer(source):
            relative = TEMPLATE_VALUE.sub("*", match.group("relative"))
            references.add((layout, match.group(1), match.group("sport"), relative))

        # Several graph layouts define the artifact root once and concatenate
        # filenames later. Scan those suffixes too so this inventory cannot be
        # bypassed by changing only the JavaScript construction style.
        for base_match in BASE_ASSIGNMENT.finditer(source):
            sport = base_match.group("sport")
            variable = re.escape(base_match.group("variable"))
            constructions = (
                re.compile(variable + r"\s*\+\s*['\"](?P<relative>[^'\"]+\.json)"),
                re.compile(r"\$\{" + variable + r"\}(?P<relative>[^`]+\.json)"),
            )
            for construction in constructions:
                for match in construction.finditer(source):
                    relative = TEMPLATE_VALUE.sub("*", match.group("relative"))
                    reference = f"python/{sport}/excel365/{match.group('relative')}"
                    references.add((layout, reference, sport, relative))
    return sorted(references)


class SiteArtifactManifestTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.manifest = json.loads(MANIFEST_PATH.read_text(encoding="utf-8"))
        cls.references = layout_references()

    def test_manifest_has_a_version_and_all_five_sports(self) -> None:
        self.assertEqual(self.manifest["schema_version"], 1)
        self.assertEqual(set(self.manifest["sports"]), SPORTS)
        self.assertGreater(len(self.references), 0)

    def test_phase_two_closure_has_no_alpine_or_cross_country_legacy_gap(self) -> None:
        milestone = self.manifest["milestones"][
            "phase_2_alpine_cross_country_site_parity"
        ]
        self.assertEqual(milestone["status"], "complete")
        self.assertEqual(set(milestone["sports"]), {"alpine", "cross-country"})

        expected_families = set(self.manifest["families"])
        for sport in milestone["sports"]:
            with self.subTest(sport=sport):
                coverage = self.manifest["sports"][sport]["coverage"]
                self.assertEqual(set(coverage), expected_families)
                self.assertTrue(
                    all(
                        self.manifest["owners"][contract["owner"]]["state"]
                        == "modernized"
                        for contract in coverage.values()
                    )
                )

    def test_phase_two_workflows_generate_test_and_publish_complete_roots(self) -> None:
        milestone = self.manifest["milestones"][
            "phase_2_alpine_cross_country_site_parity"
        ]
        for sport in milestone["sports"]:
            owner_names = {
                contract["owner"]
                for contract in self.manifest["sports"][sport]["coverage"].values()
            }
            self.assertEqual(len(owner_names), 1, sport)
            owner = self.manifest["owners"][owner_names.pop()]
            workflow = (BLOG_ROOT / owner["workflow"]).read_text(encoding="utf-8")
            module = owner["generator"].removesuffix(".py").replace("/", ".")
            artifact_root = self.manifest["sports"][sport]["artifact_root"]

            generation = workflow.index(f"python3 -m {module}")
            test_suite = workflow.index("unittest discover", generation)
            publication = workflow.index(f"git add {artifact_root}", test_suite)
            self.assertLess(generation, test_suite)
            self.assertLess(test_suite, publication)
            self.assertIn(f"--output-root {artifact_root}", workflow[generation:test_suite])

    def test_phase_five_ski_jumping_has_complete_hosted_ownership(self) -> None:
        milestone = self.manifest["milestones"][
            "phase_5_ski_jumping_site_parity"
        ]
        self.assertEqual(milestone["status"], "complete")
        self.assertEqual(milestone["sports"], ["skijump"])
        coverage = self.manifest["sports"]["skijump"]["coverage"]
        # Ski-jumping profiles consume complete regenerated chunks directly;
        # unlike Alpine/XC, no live layout consumes a separate update overlay.
        self.assertEqual(
            set(coverage),
            set(self.manifest["families"]) - {"athlete_history_updates"},
        )
        self.assertEqual(
            {contract["owner"] for contract in coverage.values()},
            {"skijump_site_data"},
        )
        owner = self.manifest["owners"]["skijump_site_data"]
        workflow = (BLOG_ROOT / owner["workflow"]).read_text(encoding="utf-8")
        generation = workflow.index("python3 -m pipelines.skijump_site_data")
        tests = workflow.index("unittest discover", generation)
        render = workflow.index("python3 -m pipelines.sport_site_smoke", tests)
        publication = workflow.index(
            "git add -- \"$artifact_root\"", render
        )
        self.assertLess(generation, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publication)

    def test_phase_six_nordic_combined_has_complete_hosted_ownership(self) -> None:
        milestone = self.manifest["milestones"][
            "phase_6_nordic_combined_site_parity"
        ]
        self.assertEqual(milestone["status"], "complete")
        self.assertEqual(milestone["sports"], ["nordic-combined"])
        coverage = self.manifest["sports"]["nordic-combined"]["coverage"]
        self.assertEqual(
            set(coverage),
            set(self.manifest["families"]) - {"athlete_history_updates"},
        )
        self.assertEqual(
            {contract["owner"] for contract in coverage.values()},
            {"nordic_combined_site_data"},
        )
        owner = self.manifest["owners"]["nordic_combined_site_data"]
        workflow = (BLOG_ROOT / owner["workflow"]).read_text(encoding="utf-8")
        generation = workflow.index(
            "python3 -m pipelines.nordic_combined_site_data"
        )
        tests = workflow.index("unittest discover", generation)
        render = workflow.index("python3 -m pipelines.sport_site_smoke", tests)
        publication = workflow.index(
            "git add -- \"$artifact_root\"", render
        )
        self.assertLess(generation, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publication)

    def test_phase_seven_biathlon_has_complete_hosted_ownership(self) -> None:
        milestone = self.manifest["milestones"][
            "phase_7_biathlon_site_parity"
        ]
        self.assertEqual(milestone["status"], "complete")
        self.assertEqual(milestone["sports"], ["biathlon"])
        coverage = self.manifest["sports"]["biathlon"]["coverage"]
        self.assertEqual(
            set(coverage),
            set(self.manifest["families"]) - {"athlete_history_updates"},
        )
        self.assertEqual(
            {contract["owner"] for contract in coverage.values()},
            {"biathlon_site_data"},
        )
        owner = self.manifest["owners"]["biathlon_site_data"]
        workflow = (BLOG_ROOT / owner["workflow"]).read_text(encoding="utf-8")
        generation = workflow.index("python3 -m pipelines.biathlon_site_data")
        tests = workflow.index("unittest discover", generation)
        render = workflow.index("python3 -m pipelines.sport_site_smoke", tests)
        publication = workflow.index('git add -- "$artifact_root"', render)
        self.assertLess(generation, tests)
        self.assertLess(tests, render)
        self.assertLess(render, publication)

    def test_every_live_layout_reference_has_exactly_one_artifact_family(self) -> None:
        families = self.manifest["families"]
        failures: list[str] = []
        observed: dict[str, set[str]] = {sport: set() for sport in SPORTS}

        for layout, reference, sport, relative in self.references:
            matches = [
                family
                for family, contract in families.items()
                if fnmatchcase(relative, contract["reference_glob"])
            ]
            if len(matches) != 1:
                failures.append(
                    f"{layout.relative_to(BLOG_ROOT)}: {reference} matched {matches}"
                )
                continue
            observed[sport].add(matches[0])

        self.assertEqual(failures, [])
        for sport, families_seen in observed.items():
            self.assertEqual(
                families_seen,
                set(self.manifest["sports"][sport]["coverage"]),
                f"{sport} manifest coverage differs from its live layout dependencies",
            )

    def test_every_referenced_family_has_existing_artifacts(self) -> None:
        for sport, contract in self.manifest["sports"].items():
            root = BLOG_ROOT / contract["artifact_root"]
            self.assertTrue(root.is_dir(), f"missing artifact root for {sport}: {root}")
            for family, coverage in contract["coverage"].items():
                artifact_glob = self.manifest["families"][family]["reference_glob"]
                materialized = any(root.glob(artifact_glob))
                if not materialized:
                    self.assertEqual(
                        coverage.get("repository_materialization"),
                        "pending_first_accepted_change",
                        f"{sport}/{family} has no artifact matching {artifact_glob}",
                    )

    def test_owners_make_legacy_gaps_explicit(self) -> None:
        owners = self.manifest["owners"]
        states = self.manifest["states"]
        for sport, sport_contract in self.manifest["sports"].items():
            for family, coverage in sport_contract["coverage"].items():
                with self.subTest(sport=sport, family=family):
                    owner = owners[coverage["owner"]]
                    self.assertIn(owner["state"], states)
                    if owner["state"] == "modernized":
                        self.assertNotIn("target_phase", coverage)
                        self.assertTrue((BLOG_ROOT / owner["workflow"]).is_file())
                        self.assertTrue((BLOG_ROOT / owner["generator"]).is_file())
                        self.assertNotEqual(owner["cadence"], "not running")
                    else:
                        self.assertEqual(coverage["owner"], "retired_master_automation")
                        self.assertIsInstance(coverage.get("target_phase"), int)
                        self.assertIsNone(owner["workflow"])
                        self.assertIsNone(owner["generator"])

    def test_family_contracts_declare_format_shape_and_legacy_origin(self) -> None:
        for family, contract in self.manifest["families"].items():
            with self.subTest(family=family):
                self.assertEqual(contract["format"], "json")
                self.assertTrue(contract["shape"])
                self.assertTrue(contract["reference_glob"].endswith(".json"))
                if family != "athlete_history_updates":
                    legacy = contract["legacy_generator"].format(sport="alpine")
                    self.assertTrue((BLOG_ROOT / legacy).is_file(), legacy)


if __name__ == "__main__":
    unittest.main()
