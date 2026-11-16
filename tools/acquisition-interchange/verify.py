"""Verify installed-R acquisition interchange against independent fixed truth."""

from __future__ import annotations

import argparse
import datetime as dt
from pathlib import Path
from typing import Any

from acquisition_profile import (
    PROFILE,
    SOURCE_RECEIPTS,
    MUTATION_SOURCE_RECEIPTS,
    QualificationError,
    _expected_tables_for_object,
    bundle_file_hashes,
    canonical_semantic_tables,
    expected_all_objects,
    expected_semantic_tables,
    load_inputs,
    mutation_completed_ids,
    read_bundle,
    read_json,
    rejection_completed_ids,
    runtime_check,
    same,
    sha256_file,
    status_check,
    progress_check,
    verify_producer,
    write_json_exclusive,
)

HERE = Path(__file__).resolve().parent


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def keys(value: Any, names: tuple[str, ...] | set[str], label: str) -> None:
    if type(value) is not dict or set(value) != set(names):
        raise QualificationError(f"Invalid {label} fields")


def hashes(root: Path, names: tuple[str, ...]) -> dict[str, str]:
    return {name: sha256_file(root / name) for name in names}


def _forward_hashes(root: Path, declaration: dict[str, Any]) -> dict[str, dict[str, str]]:
    return {
        row["id"]: bundle_file_hashes(root / row["path"])
        for row in declaration["forward"]
    }


def _mutation_index(root: Path, declaration: dict[str, Any], input_hashes: dict[str, str]) -> dict[str, Any]:
    status_check(root, "mutation-status.json", "mutate")
    progress_check(root, "mutation-progress.json", "mutate", mutation_completed_ids(declaration))
    index = read_json(root / "mutations.json", 16 * 1024 * 1024)
    keys(index, ("profile", "input_files_sha256", "source_artifacts_sha256", "invalid_bundles", "valid_bundles"), "mutation receipt")
    same(index["profile"], PROFILE, "Mutation profile")
    same(index["input_files_sha256"], input_hashes, "Mutation input hashes")
    same(index["source_artifacts_sha256"], hashes(root, MUTATION_SOURCE_RECEIPTS), "Mutation source hashes")

    invalid_specs = declaration["invalid"]
    invalid_rows = index["invalid_bundles"]
    if type(invalid_rows) is not list or len(invalid_rows) != len(invalid_specs):
        raise QualificationError("Wrong invalid bundle receipt count")
    for row, spec in zip(invalid_rows, invalid_specs, strict=True):
        keys(row, ("id", "source_case_id", "path", "mutation", "files_sha256"), "invalid bundle receipt")
        for field in ("id", "source_case_id", "path", "mutation"):
            same(row[field], spec[field], f"Invalid receipt {field}")
        same(row["files_sha256"], bundle_file_hashes(root / spec["path"]), f"Invalid bundle bytes {spec['id']}")

    control_specs = declaration["controls"]
    control_rows = index["valid_bundles"]
    if type(control_rows) is not list or len(control_rows) != len(control_specs):
        raise QualificationError("Wrong valid control receipt count")
    for row, spec in zip(control_rows, control_specs, strict=True):
        keys(row, ("id", "object_id", "kind", "path", "delimiter", "files_sha256"), "control bundle receipt")
        for field in ("id", "object_id", "kind", "path", "delimiter"):
            same(row[field], spec[field], f"Control receipt {field}")
        same(row["files_sha256"], bundle_file_hashes(root / spec["path"]), f"Control bundle bytes {spec['id']}")
    return index


def _reject_rows(rows: Any, expected_ids: list[str], label: str) -> None:
    if type(rows) is not list or len(rows) != len(expected_ids):
        raise QualificationError(f"Wrong {label} count")
    for row, ident in zip(rows, expected_ids, strict=True):
        keys(row, ("id", "error", "message"), label)
        same(row["id"], ident, f"{label} id")
        if row["error"] is not True or type(row["message"]) is not str or not row["message"].strip():
            raise QualificationError(f"No actual rejection recorded for {ident}")


def verify_rejections(
    root: Path,
    fixtures: dict[str, Any],
    declaration: dict[str, Any],
    input_hashes: dict[str, str],
    corpus_path: Path,
    produce_runtime: dict[str, Any],
) -> dict[str, Any]:
    _mutation_index(root, declaration, input_hashes)
    for spec in declaration["invalid"]:
        try:
            read_bundle(root / spec["path"])
        except (QualificationError, OSError):
            pass
        else:
            raise QualificationError(f"Independent Python parser accepted corruption {spec['id']}")

    status_check(root, "rejection-status.json", "reject")
    progress_check(root, "rejection-progress.json", "reject", rejection_completed_ids(declaration))
    reject_runtime = read_json(root / "rejection-runtime.json", 2 * 1024 * 1024)
    runtime_check(reject_runtime)
    for field in ("R", "installed_package", "dependency_versions"):
        same(reject_runtime[field], produce_runtime[field], f"Rejection runtime {field}")
    result = read_json(root / "rejections.json", 8 * 1024 * 1024)
    keys(result, ("profile", "invalid", "controls", "cross_format"), "rejection results")
    same(result["profile"], PROFILE, "Rejection result profile")

    _reject_rows(result["invalid"], [spec["id"] for spec in declaration["invalid"]], "invalid rejection")
    _reject_rows(result["cross_format"], [spec["id"] for spec in declaration["cross_format"]], "cross-format rejection")

    controls = result["controls"]
    if type(controls) is not list or len(controls) != len(declaration["controls"]):
        raise QualificationError("Wrong valid control read count")
    expected_objects = expected_all_objects(fixtures, declaration, corpus_path)
    for row, spec in zip(controls, declaration["controls"], strict=True):
        keys(row, ("id", "object_id", "kind", "observed"), "control read result")
        for field in ("id", "object_id", "kind"):
            same(row[field], spec[field], f"Control read {field}")
        same(row["observed"], expected_objects[spec["object_id"]], f"Control observation {spec['id']}")
        manifest, tables = read_bundle(root / spec["path"])
        same(manifest["Delimiter"], spec["delimiter"], f"Control delimiter {spec['id']}")
        same(canonical_semantic_tables(tables),
             expected_semantic_tables(_expected_tables_for_object(fixtures, spec["object_id"], corpus_path)),
             f"Independent control bundle {spec['id']}")
    return reject_runtime


def _forward_receipt(root: Path, fixtures: dict[str, Any], declaration: dict[str, Any], input_hashes: dict[str, str], runtime: dict[str, Any]) -> dict[str, Any]:
    source_hashes = hashes(root, SOURCE_RECEIPTS)
    result = {
        "profile": PROFILE,
        "input_files_sha256": input_hashes,
        "source_artifacts_sha256": source_hashes,
        "installed_package": runtime["installed_package"],
        "forward_bundle_files_sha256": _forward_hashes(root, declaration),
        "objects": len(fixtures["objects"]),
        "forward_bundles": len(declaration["forward"]),
        "trace_cases": len(fixtures["traces"]),
        "semantic_rejections": len(fixtures["semantic_rejections"]),
        "verified_utc": utc_now(),
    }
    return result


def _compare_forward_receipt(previous: dict[str, Any], current: dict[str, Any]) -> None:
    for field in (
        "profile", "input_files_sha256", "source_artifacts_sha256", "installed_package",
        "forward_bundle_files_sha256", "objects", "forward_bundles", "trace_cases", "semantic_rejections",
    ):
        same(previous.get(field), current[field], f"Forward verification receipt {field}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--rejections", action="store_true")
    args = parser.parse_args()
    if args.output.is_symlink() or not args.output.is_dir():
        raise SystemExit("OUTPUT must be a real existing qualification directory")
    root = args.output.resolve(strict=True)
    fixtures, declaration, input_hashes = load_inputs(HERE)
    corpus_path = HERE / "finite-stress.bin"
    runtime = verify_producer(root, fixtures, declaration, input_hashes, corpus_path)
    receipt = _forward_receipt(root, fixtures, declaration, input_hashes, runtime)
    if args.rejections:
        previous = read_json(root / "independent-python-receipt.json", 4 * 1024 * 1024)
        _compare_forward_receipt(previous, receipt)
        reject_runtime = verify_rejections(root, fixtures, declaration, input_hashes, corpus_path, runtime)
        receipt["source_artifacts_sha256"] = hashes(
            root,
            (*SOURCE_RECEIPTS, "mutations.json", "rejections.json", "rejection-runtime.json", "rejection-status.json", "rejection-progress.json"),
        )
        receipt.update(
            controls=len(declaration["controls"]),
            invalid_bundles=len(declaration["invalid"]),
            cross_format_rejections=len(declaration["cross_format"]),
            rejection_runtime=reject_runtime["installed_package"],
        )
        filename = "independent-rejection-receipt.json"
    else:
        filename = "independent-python-receipt.json"
    write_json_exclusive(root / filename, receipt)
    print(
        f"Verified {len(fixtures['objects'])} acquisition objects / {len(declaration['forward'])} forward bundles"
        + (f" / {len(declaration['controls'])} controls / {len(declaration['invalid']) + len(declaration['cross_format'])} rejections" if args.rejections else "")
    )


if __name__ == "__main__":
    main()
