"""Author independent nD controls and frozen portable corruption cases."""

import argparse
import csv
from pathlib import Path

from nd_profile import (
    expected_objects,
    load_inputs,
    read_bundle,
    read_bytes,
    read_json,
    write_bundle,
    write_json_exclusive,
)
from verify import (
    HERE,
    PROFILE,
    SOURCE_FILES,
    bundle_files,
    forward_hashes,
    hashes,
    producer,
    require,
    same,
    utc_now,
)


def manifest_read(path):
    return dict(
        line.split(": ", 1) for line in path.read_text(encoding="utf-8").splitlines()
    )


def manifest_write(path, fields):
    path.write_text(
        "".join(f"{key}: {value}\n" for key, value in fields.items()), encoding="utf-8"
    )


def tables_read(bundle):
    fields = manifest_read(bundle / "manifest.dcf")
    ext = fields["Delimiter"]
    delim = "," if ext == "csv" else "\t"
    tables = {}
    for stem in ("members", "axes", "points"):
        with (bundle / f"{stem}.{ext}").open(encoding="utf-8", newline="") as handle:
            tables[stem] = list(csv.reader(handle, delimiter=delim, strict=True))
    return fields, tables


def tables_write(bundle, fields, tables):
    ext = fields["Delimiter"]
    for stem, rows in tables.items():
        file = bundle / f"{stem}.{ext}"
        with file.open("w", encoding="utf-8", newline="") as handle:
            writer = csv.writer(
                handle,
                delimiter="," if ext == "csv" else "\t",
                quoting=csv.QUOTE_ALL,
                lineterminator="\n",
            )
            writer.writerows(rows)
        from nd_profile import sha256_file

        fields[f"{stem.title()}-Rows"] = str(len(rows) - 1)
        fields[f"{stem.title()}-Columns"] = str(len(rows[0]))
        fields[f"{stem.title()}-SHA256"] = sha256_file(file)
    manifest_write(bundle / "manifest.dcf", fields)


def copy_selected_bundle(source, destination):
    """Copy only the four bounded profile files; ignore unrelated directory entries."""
    require(
        not source.is_symlink() and source.is_dir(), "Invalid source bundle directory"
    )
    manifest_bytes = read_bytes(source / "manifest.dcf")
    fields = dict(
        line.split(": ", 1) for line in manifest_bytes.decode("utf-8").splitlines()
    )
    ext = fields["Delimiter"]
    require(ext in {"csv", "tsv"}, "Invalid source delimiter")
    names = [
        "manifest.dcf",
        *(f"{stem}.{ext}" for stem in ("members", "axes", "points")),
    ]
    payloads = {name: read_bytes(source / name) for name in names}
    require(
        sum(map(len, payloads.values())) <= 100 * 1024**2,
        "Source bundle exceeds byte limit",
    )
    destination.mkdir()
    for name, data in payloads.items():
        with (destination / name).open("xb") as handle:
            handle.write(data)


def corrupt(root, ident):
    list_cases = {
        "duplicate-member",
        "prototype-point",
        "missing-prototype",
        "nonempty-prototype",
        "unnamed-list-with-names",
    }
    base = "dad-unequal-csv" if ident in list_cases else "numeric-states-csv"
    bundle = root / "mutations" / ident
    copy_selected_bundle(root / "bundles" / base, bundle)
    fields, tables = tables_read(bundle)
    m, a, p = tables["members"], tables["axes"], tables["points"]

    # Header-relative column lookups ensure each corruption names its intended field.
    def put(table, row, field, value):
        table[row][table[0].index(field)] = value

    if ident == "duplicate-member":
        m[3][0] = m[2][0]
    elif ident == "member-gap":
        put(m, 1, "member_index", "2")
    elif ident == "orphan-axis":
        put(a, 1, "member_index", "99")
    elif ident == "duplicate-axis":
        a[2] = a[1].copy()
    elif ident == "missing-axis":
        a.pop()
    elif ident == "orphan-point":
        put(p, 1, "member_index", "99")
    elif ident == "duplicate-point":
        p[2] = p[1].copy()
    elif ident == "point-gap":
        put(p, 1, "point_index", str(len(p)))
    elif ident == "prototype-point":
        put(p, 1, "member_index", "0")
    elif ident in {"fractional-integer", "reserved-integer", "integer-overflow"}:
        token = {
            "fractional-integer": "1.5",
            "reserved-integer": "-2147483648",
            "integer-overflow": "2147483648",
        }[ident]
        put(p, 1, "location_1", token)
    elif ident in {
        "double-overflow",
        "double-underflow",
        "padded-double",
        "invalid-special-token",
    }:
        token = {
            "double-overflow": "1e9999",
            "double-underflow": "1e-9999",
            "padded-double": " 1",
            "invalid-special-token": "nan",
        }[ident]
        put(p, 1, "value", token)
    elif ident == "absent-metadata-with-text":
        put(m, 1, "dim_names_present", "0")
    elif ident == "duplicate-dimension-name":
        put(a, 2, "axis_name", a[1][a[0].index("axis_name")])
    elif ident == "blank-dimension-unit":
        put(a, 1, "axis_unit", "\u00a0\t")
    elif ident == "invalid-flag":
        put(m, 1, "dim_names_present", "2")
    elif ident == "missing-name-with-text":
        put(m, 1, "member_name", "cannot accompany missing flag")
    elif ident == "table-with-prototype":
        proto = m[1].copy()
        proto[0], proto[-1] = "0", "0"
        m.append(proto)
        for axis in a[1:].copy():
            copied = axis.copy()
            copied[0] = "0"
            a.append(copied)
    elif ident == "missing-prototype":
        m[:] = [m[0]] + [row for row in m[1:] if row[0] != "0"]
        a[:] = [a[0]] + [row for row in a[1:] if row[0] != "0"]
    elif ident == "nonempty-prototype":
        proto = next(i for i, row in enumerate(m[1:], 1) if row[0] == "0")
        put(m, proto, "point_count", "1")
    elif ident == "unnamed-list-with-names":
        fields["List-Names"] = "absent"
    elif ident == "point-count-mismatch":
        put(m, 1, "point_count", str(len(p)))
    elif ident == "huge-point-count":
        put(m, 1, "point_count", "2147483647")
    elif ident == "invalid-storage-type":
        put(a, 1, "storage_type", "logical")
    elif ident == "invalid-value-type":
        put(m, 1, "value_type", "logical")
    elif ident.startswith("wrong-") and ident.endswith("-header"):
        tables[ident[6:-7]][0][0] = "unknown_header"
    # First rehash validly encoded semantic edits, so they reach structural validation.
    tables_write(bundle, fields, tables)
    manifest = bundle / "manifest.dcf"
    points = bundle / "points.csv"
    if ident == "wrong-format":
        fields["Format"] = "measure-record"
    elif ident == "wrong-version":
        fields["Version"] = "99"
    elif ident == "dimensions-too-small":
        fields["Dimensions"] = "1"
    elif ident == "dimensions-too-large":
        fields["Dimensions"] = "1025"
    elif ident == "wrong-row-count":
        fields["Points-Rows"] = str(int(fields["Points-Rows"]) + 1)
    elif ident == "wrong-column-count":
        fields["Points-Columns"] = str(int(fields["Points-Columns"]) + 1)
    elif ident == "invalid-hash-syntax":
        fields["Points-SHA256"] = "A" * 64
    elif ident == "unknown-manifest-field":
        fields["Unrecognized"] = "1"
    elif ident == "missing-manifest-field":
        del fields["Object-Type"]
    elif ident in {
        "truncated-points",
        "malformed-quoting",
        "invalid-utf8",
        "embedded-nul",
    }:
        data = points.read_bytes()
        data = {
            "truncated-points": data[:-10],
            "malformed-quoting": data + b'"unfinished',
            "invalid-utf8": data + b"\xff",
            "embedded-nul": data + b"\0",
        }[ident]
        points.write_bytes(data)
        # Rehash malformed bytes; a generic hash mismatch must not hide parser rejection.
        from nd_profile import sha256_file

        fields["Points-SHA256"] = sha256_file(points)
    elif ident == "hash-mismatch":
        points.write_bytes(points.read_bytes() + b"\n")
    manifest_write(manifest, fields)
    if ident == "duplicate-manifest-field":
        with manifest.open("a", encoding="utf-8") as handle:
            handle.write("Version: 1\n")
    elif ident == "missing-manifest":
        manifest.unlink()
    elif ident == "missing-members":
        (bundle / "members.csv").unlink()
    return {
        "id": ident,
        "path": f"mutations/{ident}",
        "files_sha256": bundle_files(bundle),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    require(
        not args.output.is_symlink() and args.output.is_dir(),
        "OUTPUT must be a real existing directory",
    )
    root = args.output.resolve(strict=True)
    fixtures, declaration, input_hashes = load_inputs(HERE)
    expected = expected_objects(fixtures, HERE)
    runtime = producer(root, declaration, input_hashes, expected)
    receipt = read_json(root / "independent-python-receipt.json")
    same(
        receipt["source_artifacts_sha256"],
        hashes(root, SOURCE_FILES[:3]),
        "Verified producer bytes",
    )
    same(receipt["input_files_sha256"], input_hashes, "Verified input hashes")
    same(
        receipt["installed_package"],
        runtime["installed_package"],
        "Verified installed package",
    )
    same(receipt["profile"], PROFILE, "Verified profile")
    same(
        receipt["forward_bundle_files_sha256"],
        forward_hashes(root, declaration),
        "Verified forward bundle bytes",
    )
    sources = hashes(root, SOURCE_FILES)
    reserved = (
        "mutations",
        "reverse",
        "reordered",
        "mutation-status.json",
        "mutations.json",
    )
    require(
        all(
            not (root / name).exists() and not (root / name).is_symlink()
            for name in reserved
        ),
        "Mutation phase already exists",
    )
    status = {
        "phase": "mutate",
        "complete": False,
        "stage": "reserve",
        "error": None,
        "updated_utc": utc_now(),
    }
    write_json_exclusive(root / "mutation-status.json", status)
    try:
        for name in reserved[:3]:
            (root / name).mkdir()
        invalid = []
        for ident in declaration["invalid_bundles"]:
            invalid.append(corrupt(root, ident))
        valid = []
        for spec in declaration["reverse_bundles"]:
            path = root / "reverse" / spec["id"]
            write_bundle(expected[spec["object_id"]], path, spec["delimiter"])
            same(
                read_bundle(path),
                expected[spec["object_id"]],
                "Python-authored control",
            )
            valid.append(
                {
                    "id": spec["id"],
                    "object_id": spec["object_id"],
                    "kind": "reverse",
                    "path": f"reverse/{spec['id']}",
                    "files_sha256": bundle_files(path),
                }
            )
        for spec in declaration["reordered_bundles"]:
            path = root / "reordered" / spec["id"]
            copy_selected_bundle(root / "bundles" / spec["base_bundle"], path)
            fields, tables = tables_read(path)
            for rows in tables.values():
                rows[1:] = reversed(rows[1:])
            tables_write(path, fields, tables)
            same(read_bundle(path), expected[spec["object_id"]], "Reordered control")
            valid.append(
                {
                    "id": spec["id"],
                    "object_id": spec["object_id"],
                    "kind": "reordered",
                    "path": f"reordered/{spec['id']}",
                    "files_sha256": bundle_files(path),
                }
            )
        same(hashes(root, SOURCE_FILES), sources, "Unchanged producer artifacts")
        same(
            receipt["forward_bundle_files_sha256"],
            forward_hashes(root, declaration),
            "Unchanged forward bundle bytes",
        )
        write_json_exclusive(
            root / "mutations.json",
            {
                "profile": PROFILE,
                "input_files_sha256": input_hashes,
                "source_artifacts_sha256": sources,
                "invalid_bundles": invalid,
                "valid_bundles": valid,
            },
        )
        status.update(complete=True, stage="complete")
    except Exception as error:
        status.update(stage="failed", error=str(error) or type(error).__name__)
        raise
    finally:
        # This process exclusively reserved this phase status; originals are never rewritten.
        import json

        status["updated_utc"] = utc_now()
        (root / "mutation-status.json").write_text(
            json.dumps(status, indent=2, allow_nan=False) + "\n", encoding="utf-8"
        )
    print("Authored 2 reverse controls, 2 physical reorders, and 49 corrupt bundles")


if __name__ == "__main__":
    main()
