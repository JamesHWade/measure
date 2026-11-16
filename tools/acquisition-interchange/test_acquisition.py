"""Focused independent checks for the frozen acquisition oracle and mutator."""

from __future__ import annotations

import copy
import json
import subprocess
import struct
import sys
import tempfile
import unittest
import shutil
from pathlib import Path

from acquisition_profile import (
    QualificationError,
    _expected_tables_for_object,
    _parse_manifest,
    _r_blank,
    canonical_semantic_tables,
    canonical_tables,
    calculate_trace,
    decode_vector,
    expected_all_objects,
    expected_semantic_tables,
    frozen_input_check,
    load_inputs,
    loads_json,
    progress_check,
    producer_completed_ids,
    read_bundle,
    rejection_completed_ids,
    runtime_check,
    sha256_file,
    validate_declared_traces,
    write_bundle,
    write_json_exclusive,
)
import mutate

HERE = Path(__file__).resolve().parent


class AcquisitionOracleTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.fixtures, cls.declaration, cls.input_hashes = load_inputs(HERE)
        cls.corpus = HERE / "finite-stress.bin"
        cls.forward = {item["id"]: item for item in cls.declaration["forward"]}

    def test_static_traces_are_independently_derived(self) -> None:
        result = validate_declared_traces(self.fixtures, self.corpus)
        self.assertEqual(len(result), 10)
        self.assertEqual(sum(len(case["expected"]) for case in self.fixtures["traces"]), 25)
        self.assertEqual(len(self.fixtures["semantic_rejections"]), 15)
        xic = next(case for case in self.fixtures["traces"] if case["id"] == "main-xic-duplicate-boundary")
        self.assertFalse(xic["expected"][1]["saturation_unknown"])
        tic = next(case for case in self.fixtures["traces"] if case["id"] == "main-tic")
        self.assertEqual(tic["expected"][-1]["status"], "missing_intensity")

    def test_centroid_overflow_precedes_saturation_and_missing_propagation(self) -> None:
        trace = next(case for case in self.fixtures["semantic_rejections"] if case["id"] == "unrepresentable-centroid-sum")
        max_atom = {"hex": struct.pack(">d", sys.float_info.max).hex()}
        cases = (
            ([max_atom, max_atom], [True, False]),
            ([{"state": "na"}, max_atom, max_atom], [False, False, False]),
        )
        for intensities, masks in cases:
            with self.subTest(count=len(intensities), saturated=any(masks), has_na=any("state" in value for value in intensities)):
                fixtures = copy.deepcopy(self.fixtures)
                obj = next(row for row in fixtures["objects"] if row["id"] == "ms-numeric-errors")
                scan = next(row for row in obj["scans"] if row["scan_id"] == "overflow-scan")
                scan["measures"]["location"] = {
                    "storage": "double",
                    "data": [
                        {"hex": struct.pack(">d", 100.0 + index).hex()}
                        for index in range(len(intensities))
                    ],
                }
                scan["measures"]["value"] = {"storage": "double", "data": intensities}
                scan["source_rows"] = [1] * len(intensities)
                scan["saturated"] = masks
                with self.assertRaises(QualificationError):
                    calculate_trace(fixtures, trace, self.corpus)

    def test_integer_mz_range_processing_is_canonical_double(self) -> None:
        original = next(case for case in self.fixtures["traces"] if case["id"] == "main-xic-duplicate-boundary")
        integer_trace = copy.deepcopy(original)
        integer_trace["mz_range"] = {"storage": "integer", "data": [100, 100]}
        original_rows, _ = calculate_trace(self.fixtures, original, self.corpus)
        integer_rows, processing = calculate_trace(self.fixtures, integer_trace, self.corpus)
        self.assertEqual(integer_rows, original_rows)
        self.assertEqual(
            processing["mz_range"],
            {
                "storage": "double",
                "data": [
                    {"hex": struct.pack(">d", 100.0).hex()},
                    {"hex": struct.pack(">d", 100.0).hex()},
                ],
            },
        )

    def test_scalar_points_use_point_index_and_preserve_parent_zero_contract(self) -> None:
        expected = _expected_tables_for_object(self.fixtures, "ms-centroid", self.corpus)
        with tempfile.TemporaryDirectory(prefix="measure acquisition scalar order ") as temporary:
            root = Path(temporary)
            permuted = copy.deepcopy(expected)
            permuted["Points"] = [permuted["Points"][1], permuted["Points"][0], permuted["Points"][2]]
            path = root / "permuted"
            write_bundle(path, "ms-centroid", "csv", permuted)
            manifest, tables = read_bundle(path)
            self.assertEqual(canonical_tables(manifest, tables), expected)

            negative_zero = copy.deepcopy(expected)
            negative_zero["Points"][0][2] = "-0"
            negative_zero["Points"][0][3] = "-0"
            zero_expected = copy.deepcopy(expected)
            zero_expected["Points"][0][2] = "0"
            zero_expected["Points"][0][3] = "0"
            zero_path = root / "negative zero"
            write_bundle(zero_path, "ms-centroid", "csv", negative_zero)
            zero_manifest, zero_tables = read_bundle(zero_path)
            self.assertEqual(
                canonical_semantic_tables(zero_tables),
                expected_semantic_tables(zero_expected),
            )

            empty = copy.deepcopy(expected)
            empty["Points"] = []
            with self.assertRaises(QualificationError):
                write_bundle(root / "empty curve", "ms-centroid", "csv", empty)

    def test_strict_json_and_manifest_line_boundaries(self) -> None:
        with self.assertRaises(QualificationError):
            loads_json(b'{"nested":{"value":"\\ud800"}}', "surrogate probe")
        expected = _expected_tables_for_object(self.fixtures, "ms-centroid", self.corpus)
        with tempfile.TemporaryDirectory(prefix="measure acquisition dcf lines ") as temporary:
            bundle = Path(temporary) / "bundle"
            write_bundle(bundle, "ms-centroid", "csv", expected)
            manifest = (bundle / "manifest.dcf").read_bytes()
            separators = (b"\v", b"\f", b"\x1c", b"\x1d", b"\x1e", "\u0085".encode(), "\u2028".encode(), "\u2029".encode())
            for separator in separators:
                with self.subTest(separator=separator):
                    invalid = manifest[:-1].replace(b"\n", separator) + b"\n"
                    with self.assertRaises(QualificationError):
                        _parse_manifest(invalid)

    def test_progress_requires_exact_unique_string_coverage(self) -> None:
        expected = ["forward-1", "trace-1", "legacy-controls"]
        progress = {
            "phase": "produce",
            "complete": True,
            "stage": "complete",
            "current_case": None,
            "completed_bundles": expected,
            "error": None,
            "updated_utc": "2026-11-15T00:00:00Z",
        }
        with tempfile.TemporaryDirectory(prefix="measure acquisition progress ") as temporary:
            root = Path(temporary)
            path = root / "progress.json"
            write_json_exclusive(path, progress)
            progress_check(root, path.name, "produce", expected)
            for malformed in ([], [None], [{"id": "fabricated"}], ["duplicate", "duplicate"]):
                with self.subTest(completed=malformed):
                    value = {**progress, "completed_bundles": malformed}
                    path.write_text(json.dumps(value), encoding="utf-8")
                    with self.assertRaises(QualificationError):
                        progress_check(root, path.name, "produce", expected)

    def test_corpus_is_frozen_big_endian_binary64(self) -> None:
        finite = next(item for item in self.fixtures["objects"] if item["id"] == "finite-stress")
        vector = finite["arrays"][0]["measures"]["value"]
        storage, values = decode_vector(vector, "corpus", self.corpus)
        self.assertEqual(storage, "double")
        self.assertEqual(len(values), 8208)
        self.assertEqual([state for state, _value in values[:2]], ["hex", "hex"])
        import struct

        self.assertEqual(struct.pack(">d", values[0][1]).hex(), "0000000000000000")
        self.assertEqual(struct.pack(">d", values[1][1]).hex(), "8000000000000000")

    def test_python_writer_and_reader_roundtrip_all_forward_fixtures(self) -> None:
        with tempfile.TemporaryDirectory(prefix="measure acquisition oracle ") as temporary:
            root = Path(temporary)
            for spec in self.declaration["forward"]:
                with self.subTest(bundle=spec["id"]):
                    expected = _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus)
                    path = root / spec["id"]
                    write_bundle(path, spec["object_id"], spec["delimiter"], expected)
                    manifest, tables = read_bundle(path)
                    self.assertEqual(manifest["Delimiter"], spec["delimiter"])
                    self.assertEqual(canonical_tables(manifest, tables), expected)

    def test_r_runtime_paths_and_r_blank_semantics(self) -> None:
        version = "0.0.1.9003"
        r_version = "R version 4.6.2 (2026-10-29)"
        for path in (
            "/opt/R library/measure",
            "C:/R libraries/measure",
            "D:/Users/researcher/R libraries/measure",
            "//server/share/R library/measure",
        ):
            runtime = {
                "profile": "measure-acquisition/1",
                "R": r_version,
                "installed_package": {
                    "path": path,
                    "namespace_path": path,
                    "version": version,
                    "built": None,
                    "remote_sha": None,
                },
                "tool_checkout_head": None,
                "created_utc": "2026-11-15T00:00:00Z",
                "session_info": [r_version],
                "dependency_versions": {
                    "measure": version,
                    "vctrs": "0.6.5",
                    "jsonlite": "2.0.0",
                    "digest": "0.6.37",
                    "fs": "1.6.6",
                },
            }
            with self.subTest(path=path):
                runtime_check(runtime)
        self.assertTrue(_r_blank("\u180e"))
        self.assertFalse(_r_blank("\u001c"))
        self.assertFalse("\u180e".isspace())
        self.assertTrue("\u001c".isspace())

    def test_frozen_output_inputs_match_all_five_source_files(self) -> None:
        with tempfile.TemporaryDirectory(prefix="measure acquisition copied inputs ") as temporary:
            output = Path(temporary)
            frozen = output / "frozen-inputs"
            frozen.mkdir()
            for name in (*self.input_hashes, "input-manifest.json"):
                shutil.copyfile(HERE / name, frozen / name)
            frozen_input_check(output, HERE, self.input_hashes)
            (frozen / "fixtures.json").write_bytes(b"changed\n")
            with self.assertRaises(QualificationError):
                frozen_input_check(output, HERE, self.input_hashes)

    def test_all_declared_corruptions_are_independently_rejected(self) -> None:
        source_ids = {item["source_case_id"] for item in self.declaration["invalid"]}
        with tempfile.TemporaryDirectory(prefix="measure acquisition mutations ") as temporary:
            root = Path(temporary)
            (root / "bundles").mkdir()
            (root / "mutants").mkdir()
            for source_id in source_ids:
                spec = self.forward[source_id]
                write_bundle(
                    root / "bundles" / source_id,
                    spec["object_id"],
                    spec["delimiter"],
                    _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus),
                )
            for spec in self.declaration["invalid"]:
                with self.subTest(mutation=spec["id"]):
                    source = root / "bundles" / spec["source_case_id"]
                    destination = root / spec["path"]
                    mutate.copy_bundle(source, destination)
                    delimiter = spec["source_case_id"].rsplit("-", 1)[1]
                    mutate._corrupt(destination, spec["mutation"], delimiter)
                    with self.assertRaises(QualificationError):
                        read_bundle(destination)

    def test_reverse_and_physical_reorder_controls_decode_to_same_truth(self) -> None:
        source_ids = {item["source_case_id"] for item in self.declaration["controls"] if item["kind"] == "reorder"}
        with tempfile.TemporaryDirectory(prefix="measure acquisition controls ") as temporary:
            root = Path(temporary)
            (root / "bundles").mkdir()
            (root / "controls").mkdir()
            for source_id in source_ids:
                spec = self.forward[source_id]
                write_bundle(
                    root / "bundles" / source_id,
                    spec["object_id"],
                    spec["delimiter"],
                    _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus),
                )
            for spec in self.declaration["controls"]:
                with self.subTest(control=spec["id"]):
                    destination = root / spec["path"]
                    if spec["kind"] == "reverse":
                        expected = _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus)
                        write_bundle(destination, spec["object_id"], spec["delimiter"], expected)
                    else:
                        mutate.copy_bundle(root / "bundles" / spec["source_case_id"], destination)
                        fields, tables = read_bundle(destination)
                        for rows in tables.values():
                            if len(rows) > 2:
                                rows[1:] = list(reversed(rows[1:]))
                        mutate._write_mutated_tables(destination, fields, tables, spec["delimiter"])
                        expected = _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus)
                    manifest, tables = read_bundle(destination)
                    self.assertEqual(canonical_tables(manifest, tables), expected)

    def test_synthetic_receipt_protocol_and_phase_handoff(self) -> None:
        """Exercise receipt wiring without claiming this as an R producer run."""
        version = "0.0.1.9003"
        r_version = "R version 4.6.2 (2026-10-29)"
        runtime = {
            "profile": "measure-acquisition/1",
            "R": r_version,
            "installed_package": {
                "path": "/private/tmp/fake library/measure",
                "namespace_path": "/private/tmp/fake library/measure",
                "version": version,
                "built": None,
                "remote_sha": None,
            },
            "tool_checkout_head": None,
            "created_utc": "2026-11-15T00:00:00Z",
            "session_info": [r_version],
            "dependency_versions": {
                "measure": version,
                "vctrs": "0.6.5",
                "jsonlite": "2.0.0",
                "digest": "0.6.37",
                "fs": "1.6.6",
            },
        }
        expected = expected_all_objects(self.fixtures, self.declaration, self.corpus)
        with tempfile.TemporaryDirectory(prefix="measure acquisition synthetic R flow ") as temporary:
            root = Path(temporary)
            frozen = root / "frozen-inputs"
            frozen.mkdir()
            for name in (*self.input_hashes, "input-manifest.json"):
                shutil.copyfile(HERE / name, frozen / name)
            write_json_exclusive(
                root / "qualification-root.json",
                {
                    "format": "measure-acquisition/1",
                    "declaration_sha256": self.input_hashes["declaration.json"],
                    "input_manifest_sha256": sha256_file(HERE / "input-manifest.json"),
                },
            )
            timestamp = "2026-11-15T00:00:00Z"
            status = {"phase": "produce", "complete": True, "stage": "complete", "error": None, "updated_utc": timestamp}
            progress = {"phase": "produce", "complete": True, "stage": "complete", "current_case": None, "completed_bundles": producer_completed_ids(self.fixtures, self.declaration), "error": None, "updated_utc": timestamp}
            write_json_exclusive(root / "run-status.json", status)
            write_json_exclusive(root / "progress.json", progress)
            write_json_exclusive(root / "runtime.json", runtime)
            write_json_exclusive(root / "observations.json", {"profile": "measure-acquisition/1", "objects": list(expected.values())})
            traces = []
            for case in self.fixtures["traces"]:
                rows, processing = validate_declared_traces(self.fixtures, self.corpus)[case["id"]]
                processing["package_version"] = version
                traces.append({"id": case["id"], "object_id": case["object_id"], "rows": rows, "processing": processing})
            write_json_exclusive(root / "trace-results.json", {"profile": "measure-acquisition/1", "cases": traces})
            rejections = [{"id": row["id"], "error": True, "message": "synthetic declared rejection"} for row in self.fixtures["semantic_rejections"]]
            write_json_exclusive(root / "semantic-rejections.json", {"profile": "measure-acquisition/1", "cases": rejections})
            for spec in self.declaration["forward"]:
                write_bundle(root / spec["path"], spec["object_id"], spec["delimiter"], _expected_tables_for_object(self.fixtures, spec["object_id"], self.corpus))

            alternate = self.forward["ms-centroid-csv"]
            bundle_path = root / alternate["path"]
            fields, tables = read_bundle(bundle_path)
            location_column = 2
            location_token = tables["ScanPoints"][1][location_column]
            self.assertNotIn(".", location_token)
            tables["ScanPoints"][1][location_column] = f"{location_token}.0"
            mutate._write_mutated_tables(bundle_path, fields, tables, "csv")

            subprocess.run([sys.executable, str(HERE / "verify.py"), str(root)], check=True, capture_output=True, text=True)
            subprocess.run([sys.executable, str(HERE / "mutate.py"), str(root)], check=True, capture_output=True, text=True)
            control_rows = [
                {"id": spec["id"], "object_id": spec["object_id"], "kind": spec["kind"], "observed": expected[spec["object_id"]]}
                for spec in self.declaration["controls"]
            ]
            invalid_rows = [{"id": spec["id"], "error": True, "message": "synthetic native rejection"} for spec in self.declaration["invalid"]]
            cross_rows = [{"id": spec["id"], "error": True, "message": "synthetic cross-format rejection"} for spec in self.declaration["cross_format"]]
            write_json_exclusive(root / "rejections.json", {"profile": "measure-acquisition/1", "invalid": invalid_rows, "controls": control_rows, "cross_format": cross_rows})
            write_json_exclusive(root / "rejection-runtime.json", runtime)
            write_json_exclusive(root / "rejection-status.json", {**status, "phase": "reject"})
            write_json_exclusive(root / "rejection-progress.json", {**progress, "phase": "reject", "completed_bundles": rejection_completed_ids(self.declaration)})
            subprocess.run([sys.executable, str(HERE / "verify.py"), str(root), "--rejections"], check=True, capture_output=True, text=True)


if __name__ == "__main__":
    unittest.main()
