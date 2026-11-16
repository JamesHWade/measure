"""Create independently authored acquisition controls and fixed invalid bundles."""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import io
import json
import os
import tempfile
from pathlib import Path
from typing import Any

from acquisition_profile import (
    PROFILE,
    QualificationError,
    MUTATION_SOURCE_RECEIPTS,
    SOURCE_RECEIPTS,
    TABLES,
    _expected_tables_for_object,
    _parse_manifest,
    bundle_file_hashes,
    copy_bundle,
    load_inputs,
    producer_completed_ids,
    read_bundle,
    read_json,
    runtime_check,
    same,
    sha256_bytes,
    sha256_file,
    status_check,
    progress_check,
    write_bundle,
    write_json_exclusive,
)

HERE = Path(__file__).resolve().parent
SOURCE_HASH_NAMES = MUTATION_SOURCE_RECEIPTS


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def _status(path: Path, phase: str, complete: bool, stage: str, error: str | None, current: str | None, done: list[str]) -> dict[str, Any]:
    return {
        "phase": phase,
        "complete": complete,
        "stage": stage,
        "error": error,
        "updated_utc": utc_now(),
        "current_case": current,
        "completed_bundles": done.copy(),
    }


def _replace_json(path: Path, value: Any) -> None:
    payload = json.dumps(value, ensure_ascii=False, allow_nan=False, indent=2, separators=(",", ": ")).encode("utf-8") + b"\n"
    fd, temp_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temp_name, path)
    except Exception:
        try:
            os.unlink(temp_name)
        except OSError:
            pass
        raise


def _write_phase_state(root: Path, complete: bool, stage: str, error: str | None, current: str | None, done: list[str]) -> None:
    status_path = root / "mutation-status.json"
    progress_path = root / "mutation-progress.json"
    status_value = {
        "phase": "mutate",
        "complete": complete,
        "stage": stage,
        "error": error,
        "updated_utc": utc_now(),
    }
    progress_value = _status(progress_path, "mutate", complete, stage, error, current, done)
    if status_path.exists() or status_path.is_symlink():
        _replace_json(status_path, status_value)
    else:
        write_json_exclusive(status_path, status_value)
    if progress_path.exists() or progress_path.is_symlink():
        _replace_json(progress_path, progress_value)
    else:
        write_json_exclusive(progress_path, progress_value)


def _manifest_fields(bundle: Path) -> dict[str, str]:
    raw = (bundle / "manifest.dcf").read_bytes()
    return _parse_manifest(raw)


def _serialize_table(rows: list[list[str]], delimiter: str) -> bytes:
    output = io.StringIO(newline="")
    writer = csv.writer(output, delimiter=delimiter, quoting=csv.QUOTE_ALL, lineterminator="\n")
    writer.writerows(rows)
    return output.getvalue().encode("utf-8", errors="strict")


def _write_mutated_tables(bundle: Path, fields: dict[str, str], tables: dict[str, list[list[str]]], delimiter_name: str) -> None:
    delimiter = "," if delimiter_name == "csv" else "\t"
    for prefix, (stem, _headers) in TABLES.items():
        payload = _serialize_table(tables[prefix], delimiter)
        path = bundle / f"{stem}.{delimiter_name}"
        path.write_bytes(payload)
        fields[f"{prefix}-Rows"] = str(max(0, len(tables[prefix]) - 1))
        fields[f"{prefix}-Columns"] = str(len(tables[prefix][0]))
        fields[f"{prefix}-SHA256"] = sha256_bytes(payload)
    _write_manifest(bundle / "manifest.dcf", fields)


def _write_manifest(path: Path, fields: dict[str, str]) -> None:
    from acquisition_profile import MANIFEST_FIELDS

    ordered = [(name, fields[name]) for name in MANIFEST_FIELDS if name in fields]
    if set(fields) != set(MANIFEST_FIELDS):
        raise QualificationError("Mutated manifest has an unexpected field set")
    path.write_bytes("".join(f"{key}: {value}\n" for key, value in ordered).encode("utf-8"))


def _set(tables: dict[str, list[list[str]]], prefix: str, row: int, field: str, value: str) -> None:
    rows = tables[prefix]
    try:
        column = rows[0].index(field)
    except ValueError as exc:
        raise QualificationError(f"Mutation field not found: {prefix}.{field}") from exc
    rows[row][column] = value


def _index(tables: dict[str, list[list[str]]], prefix: str, field: str) -> int:
    return tables[prefix][0].index(field)


def _raw_replace(bundle: Path, stem: str, extension: str, update: Any, fields: dict[str, str], prefix: str) -> None:
    path = bundle / f"{stem}.{extension}"
    data = path.read_bytes()
    data = update(data)
    path.write_bytes(data)
    fields[f"{prefix}-SHA256"] = sha256_bytes(data)
    _write_manifest(bundle / "manifest.dcf", fields)


def _corrupt(bundle: Path, ident: str, extension: str) -> None:
    fields = _manifest_fields(bundle)
    _parsed_manifest, tables = read_bundle(bundle)
    tables = {key: [row.copy() for row in rows] for key, rows in tables.items()}
    prefix_for_headers = {
        "wrong-header-order": "Streams",
        "duplicate-column-header": "Samples",
        "extra-column": "Samples",
    }
    if ident == "wrong-format":
        fields["Format"] = "measure-record"
    elif ident == "wrong-version":
        fields["Version"] = "99"
    elif ident == "duplicate-manifest-field":
        path = bundle / "manifest.dcf"
        path.write_bytes(path.read_bytes() + b"Format: measure-acquisition\n")
        return
    elif ident == "unknown-manifest-field":
        path = bundle / "manifest.dcf"
        path.write_bytes(path.read_bytes() + b"Unexpected: 1\n")
        return
    elif ident == "missing-manifest-field":
        del fields["Sources-SHA256"]
        _write_manifest_partial(bundle / "manifest.dcf", fields)
        return
    elif ident == "invalid-hash-syntax":
        fields["Points-SHA256"] = "A" * 64
    elif ident == "hash-mismatch":
        path = bundle / f"samples.{extension}"
        payload = path.read_bytes()
        path.write_bytes(payload + b" ")
        return
    elif ident == "row-count-mismatch":
        fields["Scans-Rows"] = str(int(fields["Scans-Rows"]) + 1)
    elif ident == "column-count-mismatch":
        fields["ScanPoints-Columns"] = str(int(fields["ScanPoints-Columns"]) + 1)
    elif ident == "missing-manifest":
        (bundle / "manifest.dcf").unlink()
        return
    elif ident == "missing-payload":
        (bundle / f"arrays.{extension}").unlink()
        return
    elif ident == "wrong-delimiter":
        stem = "streams"
        prefix = "Streams"
        _raw_replace(bundle, stem, extension, lambda data: data.replace(b",", b"\t"), fields, prefix)
        return
    elif ident in prefix_for_headers:
        prefix = prefix_for_headers[ident]
        rows = tables[prefix]
        if ident == "wrong-header-order":
            rows[0][0], rows[0][1] = rows[0][1], rows[0][0]
        elif ident == "duplicate-column-header":
            rows[0][1] = rows[0][0]
        else:
            rows[0].append("unexpected")
            for row in rows[1:]:
                row.append("")
        _write_mutated_tables(bundle, fields, tables, extension)
        return
    elif ident == "malformed-quoting":
        _raw_replace(bundle, "samples", extension, lambda _data: b'"row_index"junk,sample_id\n"1","sample\n', fields, "Samples")
        return
    elif ident == "invalid-utf8":
        _raw_replace(bundle, "channels", extension, lambda data: data + b"\xff", fields, "Channels")
        return
    elif ident == "embedded-nul":
        _raw_replace(bundle, "channels", extension, lambda data: data.replace(b"channel_name", b"channel\x00name", 1), fields, "Channels")
        return
    elif ident == "duplicate-sample-row-index":
        _set(tables, "Samples", 2, "row_index", "1")
    elif ident == "metadata-row-gap":
        _set(tables, "Samples", 2, "row_index", "3")
    elif ident == "duplicate-scan-id":
        _set(tables, "Scans", 2, "scan_id", tables["Scans"][1][1])
    elif ident == "scan-index-zero":
        _set(tables, "Scans", 1, "scan_index", "0")
    elif ident == "scan-point-orphan":
        _set(tables, "ScanPoints", 1, "scan_id", "unknown-scan")
    elif ident == "scan-point-duplicate-index":
        _set(tables, "ScanPoints", 2, "point_index", "1")
    elif ident == "scan-point-gap":
        _set(tables, "ScanPoints", 2, "point_index", "5")
    elif ident == "unknown-stream-reference":
        _set(tables, "Scans", 1, "stream_id", "unknown-stream")
    elif ident == "scan-point-count-mismatch":
        count = int(tables["Scans"][1][_index(tables, "Scans", "point_count")])
        _set(tables, "Scans", 1, "point_count", str(count + 1))
    elif ident == "scan-location-type-invalid":
        _set(tables, "Scans", 1, "location_type", "logical")
    elif ident == "scan-value-type-invalid":
        _set(tables, "Scans", 1, "value_type", "logical")
    elif ident == "source-row-zero":
        _set(tables, "ScanPoints", 1, "source_row", "0")
    elif ident == "invalid-saturation-token":
        _set(tables, "ScanPoints", 1, "saturated", "true")
    elif ident == "double-token-invalid":
        _set(tables, "ScanPoints", 1, "location", "nan")
    elif ident == "double-overflow":
        _set(tables, "ScanPoints", 1, "location", "1e9999")
    elif ident == "double-underflow":
        _set(tables, "ScanPoints", 1, "location", "1e-9999")
    elif ident == "integer-overflow":
        _set(tables, "ScanPoints", 1, "source_row", "2147483648")
    elif ident == "duplicate-array-id":
        _set(tables, "Arrays", 2, "array_id", tables["Arrays"][1][1])
    elif ident == "array-axis-orphan":
        _set(tables, "ArrayAxes", 1, "array_id", "unknown-array")
    elif ident == "array-axis-duplicate":
        _set(tables, "ArrayAxes", 2, "axis_index", "1")
    elif ident == "missing-array-axis":
        tables["ArrayAxes"].pop(2)
    elif ident == "array-point-orphan":
        _set(tables, "ArrayPoints", 1, "array_id", "unknown-array")
    elif ident == "array-point-count-mismatch":
        count = int(tables["Arrays"][1][_index(tables, "Arrays", "point_count")])
        _set(tables, "Arrays", 1, "point_count", str(count + 1))
    elif ident == "array-location-storage-invalid":
        _set(tables, "ArrayAxes", 1, "storage_type", "logical")
    elif ident == "array-value-type-invalid":
        _set(tables, "Arrays", 1, "value_type", "logical")
    elif ident == "array-dim-flag-invalid":
        _set(tables, "Arrays", 1, "dim_names_present", "2")
    elif ident == "absent-dimension-with-text":
        _set(tables, "Arrays", 1, "dim_names_present", "0")
    elif ident == "array-double-token-invalid":
        _set(tables, "ArrayPoints", 1, "location_2", "nan")
    else:
        raise QualificationError(f"Unknown mutation id: {ident}")

    _write_mutated_tables(bundle, fields, tables, extension)
    if ident == "invalid-hash-syntax":
        fields["Points-SHA256"] = "A" * 64
    elif ident == "row-count-mismatch":
        fields["Scans-Rows"] = str(int(fields["Scans-Rows"]) + 1)
    elif ident == "column-count-mismatch":
        fields["ScanPoints-Columns"] = str(int(fields["ScanPoints-Columns"]) + 1)
    if ident in {"wrong-format", "wrong-version", "invalid-hash-syntax", "row-count-mismatch", "column-count-mismatch"}:
        _write_manifest(bundle / "manifest.dcf", fields)


def _write_manifest_partial(path: Path, fields: dict[str, str]) -> None:
    from acquisition_profile import MANIFEST_FIELDS

    path.write_bytes("".join(f"{key}: {fields[key]}\n" for key in MANIFEST_FIELDS if key in fields).encode("utf-8"))


def _record_bundle_files(path: Path) -> dict[str, str]:
    return bundle_file_hashes(path)


def _verify_forward_receipt(root: Path, declaration: dict[str, Any], input_hashes: dict[str, str]) -> None:
    receipt = read_json(root / "independent-python-receipt.json", 4 * 1024 * 1024)
    expected_keys = (
        "profile", "input_files_sha256", "source_artifacts_sha256", "installed_package",
        "forward_bundle_files_sha256", "objects", "forward_bundles", "trace_cases",
        "semantic_rejections", "verified_utc",
    )
    if type(receipt) is not dict or set(receipt) != set(expected_keys):
        raise QualificationError("Forward Python verification receipt has invalid fields")
    same(receipt["profile"], PROFILE, "Forward Python receipt profile")
    same(receipt["input_files_sha256"], input_hashes, "Forward Python receipt inputs")
    same(receipt["source_artifacts_sha256"], {
        name: sha256_file(root / name) for name in SOURCE_RECEIPTS
    }, "Forward Python receipt source artifacts")
    same(receipt["forward_bundle_files_sha256"], {
        row["id"]: bundle_file_hashes(root / row["path"]) for row in declaration["forward"]
    }, "Forward Python receipt bundle hashes")
    same(receipt["objects"], len(declaration["object_ids"]), "Forward Python object count")
    same(receipt["forward_bundles"], len(declaration["forward"]), "Forward Python bundle count")
    same(receipt["trace_cases"], len(declaration["trace_ids"]), "Forward Python trace count")
    same(receipt["semantic_rejections"], len(declaration["semantic_rejection_ids"]), "Forward Python rejection count")
    if type(receipt["verified_utc"]) is not str or not receipt["verified_utc"]:
        raise QualificationError("Forward Python receipt has no verification timestamp")
    runtime = read_json(root / "runtime.json", 2 * 1024 * 1024)
    runtime_check(runtime)
    same(receipt["installed_package"], runtime["installed_package"], "Forward Python installed package")


def mutate(root: Path) -> None:
    if root.is_symlink() or not root.is_dir():
        raise QualificationError("OUTPUT must be an existing real directory")
    root = root.resolve(strict=True)
    fixtures, declaration, input_hashes = load_inputs(HERE)
    status_check(root, "run-status.json", "produce")
    progress_check(root, "progress.json", "produce", producer_completed_ids(fixtures, declaration))
    # Require the independent producer verification before creating controls.
    _verify_forward_receipt(root, declaration, input_hashes)
    for path in (root / "mutations.json", root / "mutation-status.json", root / "mutation-progress.json", root / "controls", root / "mutants"):
        if path.exists() or path.is_symlink():
            raise QualificationError(f"Mutate phase output already exists: {path}")
    (root / "controls").mkdir()
    (root / "mutants").mkdir()
    done: list[str] = []
    _write_phase_state(root, False, "initialized", None, None, done)
    try:
        for spec in declaration["controls"]:
            _write_phase_state(root, False, "control", None, spec["id"], done)
            destination = root / spec["path"]
            if spec["kind"] == "reverse":
                tables = _expected_tables_for_object(fixtures, spec["object_id"], HERE / "finite-stress.bin")
                write_bundle(destination, spec["object_id"], spec["delimiter"], tables)
            else:
                source = root / "bundles" / spec["source_case_id"]
                copy_bundle(source, destination)
                fields, tables = read_bundle(destination)
                for prefix in tables:
                    if len(tables[prefix]) > 2:
                        tables[prefix][1:] = list(reversed(tables[prefix][1:]))
                _write_mutated_tables(destination, fields, tables, spec["delimiter"])
                read_bundle(destination)
            done.append(spec["id"])
            _write_phase_state(root, False, "control", None, None, done)

        for spec in declaration["invalid"]:
            _write_phase_state(root, False, "invalid", None, spec["id"], done)
            source = root / "bundles" / spec["source_case_id"]
            destination = root / spec["path"]
            copy_bundle(source, destination)
            _corrupt(destination, spec["mutation"], spec["delimiter"] if "delimiter" in spec else read_bundle(source)[0]["Delimiter"])
            done.append(spec["id"])
            _write_phase_state(root, False, "invalid", None, None, done)

        original_hashes = {name: sha256_file(root / name) for name in SOURCE_HASH_NAMES}
        invalid_records = [
            {
                "id": spec["id"],
                "source_case_id": spec["source_case_id"],
                "path": spec["path"],
                "mutation": spec["mutation"],
                "files_sha256": _record_bundle_files(root / spec["path"]),
            }
            for spec in declaration["invalid"]
        ]
        control_records = [
            {
                "id": spec["id"],
                "object_id": spec["object_id"],
                "kind": spec["kind"],
                "path": spec["path"],
                "delimiter": spec["delimiter"],
                "files_sha256": _record_bundle_files(root / spec["path"]),
            }
            for spec in declaration["controls"]
        ]
        receipt = {
            "profile": PROFILE,
            "input_files_sha256": input_hashes,
            "source_artifacts_sha256": original_hashes,
            "invalid_bundles": invalid_records,
            "valid_bundles": control_records,
        }
        write_json_exclusive(root / "mutations.json", receipt)
        done = [*done, "mutations.json"]
        _write_phase_state(root, True, "complete", None, None, done)
    except Exception as exc:
        _write_phase_state(root, False, "failed", str(exc) or type(exc).__name__, None, done)
        raise


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    mutate(args.output)
    print("Created 4 controls and 46 fixed invalid acquisition bundles")


if __name__ == "__main__":
    main()
