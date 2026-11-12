"""Independently verify fixed record bundles with only Python's standard library."""

import csv
import hashlib
import json
import struct
import sys
from pathlib import Path

if not __debug__:
    raise SystemExit(
        "Run this verifier without Python optimization so its checks remain active."
    )

SCRIPT_DIR = Path(__file__).resolve().parent
PROFILE = "measure-record-interchange-qualification/1"
DECLARATION_PATH = SCRIPT_DIR / "qualification-declaration.json"
INPUT_MANIFEST_PATH = SCRIPT_DIR / "qualification-input-manifest.json"
DECLARATION = json.loads(DECLARATION_PATH.read_text(encoding="utf-8"))
INPUT_MANIFEST = json.loads(INPUT_MANIFEST_PATH.read_text(encoding="utf-8"))

TABLE_STEMS = ("sources", "samples", "injections", "channels", "curves", "points")
HEADERS = {
    "sources": ["source_id", "source_name", "source_sha256"],
    "samples": ["sample_id"],
    "injections": ["injection_id", "sample_id"],
    "channels": ["channel_id", "channel_name"],
    "curves": [
        "curve_id",
        "injection_id",
        "channel_id",
        "source_id",
        "axis_name",
        "axis_unit",
        "value_name",
        "value_unit",
    ],
    "points": ["curve_id", "point_index", "location", "value", "source_row"],
}


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def verify_inputs() -> None:
    assert digest(DECLARATION_PATH) == INPUT_MANIFEST["declaration_sha256"]
    for name, info in INPUT_MANIFEST["source_files"].items():
        path = SCRIPT_DIR / "qualification-input" / name
        assert not path.is_symlink() and path.is_file()
        assert path.stat().st_size == info["bytes"]
        assert digest(path) == info["sha256"]


def owned_root(path_text: str) -> Path:
    path = Path(path_text).expanduser()
    if path.is_symlink() or not path.is_dir():
        raise SystemExit(
            "OUTPUT must be an existing qualification directory, not a symlink."
        )
    root = path.resolve(strict=True)
    marker_path = root / "qualification-root.json"
    if marker_path.is_symlink() or not marker_path.is_file():
        raise SystemExit("OUTPUT is missing its qualification-root ownership marker.")
    marker = json.loads(marker_path.read_text(encoding="utf-8"))
    expected_marker = {
        "format": PROFILE,
        "declaration_sha256": digest(DECLARATION_PATH),
        "input_manifest_sha256": digest(INPUT_MANIFEST_PATH),
    }
    assert marker == expected_marker, (
        "qualification-root marker does not match these inputs"
    )
    return root


def read_bundle(
    bundle: Path, extension: str
) -> tuple[dict[str, str], dict[str, list[dict[str, str]]]]:
    assert extension in {"csv", "tsv"}
    assert not bundle.is_symlink() and bundle.is_dir()
    manifest_path = bundle / "manifest.dcf"
    assert not manifest_path.is_symlink() and manifest_path.is_file()
    manifest: dict[str, str] = {}
    for line in manifest_path.read_text(encoding="utf-8").splitlines():
        key, value = line.split(":", 1)
        assert key not in manifest
        manifest[key] = value.strip()

    expected_keys = {"Format", "Version", "Delimiter"}
    for stem in TABLE_STEMS:
        expected_keys.update(
            f"{stem.title()}-{suffix}" for suffix in ("Rows", "Columns", "SHA256")
        )
    assert set(manifest) == expected_keys
    assert manifest["Format"] == "measure-record"
    assert manifest["Version"] == "1"
    assert manifest["Delimiter"] == extension

    delimiter = "," if extension == "csv" else "\t"
    tables: dict[str, list[dict[str, str]]] = {}
    for stem in TABLE_STEMS:
        path = bundle / f"{stem}.{extension}"
        assert not path.is_symlink() and path.is_file()
        assert digest(path) == manifest[f"{stem.title()}-SHA256"]
        with path.open(encoding="utf-8", newline="") as stream:
            reader = csv.DictReader(stream, delimiter=delimiter, strict=True)
            assert reader.fieldnames == HEADERS[stem]
            rows = list(reader)
        for row in rows:
            assert None not in row
            assert all(value is not None for value in row.values())
        tables[stem] = rows
        assert len(rows) == int(manifest[f"{stem.title()}-Rows"])
        assert len(HEADERS[stem]) == int(manifest[f"{stem.title()}-Columns"])
    return manifest, tables


def exact_double(text: str, expected: float | None) -> None:
    if expected is None:
        assert text == "", (text, expected)
    else:
        actual = float(text)
        assert struct.pack("!d", actual) == struct.pack("!d", expected), (
            text,
            expected,
        )


def verify_fixture_bundle(
    bundle: Path, source_name: str, extension: str
) -> dict[str, object]:
    manifest, tables = read_bundle(bundle, extension)
    assert tables["sources"] == [
        {
            "source_id": DECLARATION["source_id"],
            "source_name": source_name,
            "source_sha256": INPUT_MANIFEST["source_files"][source_name]["sha256"],
        }
    ]
    for table, field, expected_field in (
        ("samples", "sample_id", "expected_sample_order"),
        ("injections", "injection_id", "expected_injection_order"),
        ("channels", "channel_id", "expected_channel_order"),
        ("curves", "curve_id", "expected_curve_order"),
    ):
        assert [row[field] for row in tables[table]] == DECLARATION[expected_field]
    assert [row["sample_id"] for row in tables["injections"]] == ["0007", "NA", "0007"]
    assert [row["channel_name"] for row in tables["channels"]] == [
        "UV, 254 nm",
        "Extracted trace\nchannel",
    ]
    assert len(tables["points"]) == INPUT_MANIFEST["logical_data_rows"]

    offset = 0
    for observed, expected in zip(tables["curves"], DECLARATION["curves"], strict=True):
        scalar = {
            key: expected[key] or "" for key in HEADERS["curves"] if key != "source_id"
        }
        scalar["source_id"] = DECLARATION["source_id"]
        assert observed == scalar, (observed, scalar)
        count = len(expected["location"])
        points = tables["points"][offset : offset + count]
        for index, row in enumerate(points):
            assert row["curve_id"] == expected["curve_id"]
            assert row["point_index"] == str(index + 1)
            assert row["source_row"] == str(expected["source_rows"][index])
            exact_double(row["location"], expected["location"][index])
            exact_double(row["value"], expected["value"][index])
        offset += count
    return {
        "bundle": str(bundle),
        "source": source_name,
        "delimiter": extension,
        "curves": len(tables["curves"]),
        "points": offset,
        "exact_numeric_and_metadata_match": True,
        "payload_sha256": {
            stem: manifest[f"{stem.title()}-SHA256"] for stem in TABLE_STEMS
        },
    }


def verify_numeric_bundle(
    bundle: Path,
    extension: str,
    oracle: bytes,
    values: int,
) -> dict[str, object]:
    manifest, tables = read_bundle(bundle, extension)
    assert tables["sources"] == []
    assert tables["samples"] == [{"sample_id": "numeric-corpus"}]
    assert tables["injections"] == [
        {"injection_id": "injection-1", "sample_id": "numeric-corpus"}
    ]
    assert tables["channels"] == [{"channel_id": "channel-1", "channel_name": ""}]
    assert tables["curves"] == [
        {
            "curve_id": "corpus",
            "injection_id": "injection-1",
            "channel_id": "channel-1",
            "source_id": "",
            "axis_name": "",
            "axis_unit": "",
            "value_name": "",
            "value_unit": "",
        }
    ]
    points = tables["points"]
    assert len(points) == values
    assert len(points) == int(manifest["Points-Rows"])
    assert len(oracle) == values * 8

    for index, row in enumerate(points):
        assert row["curve_id"] == "corpus"
        assert row["point_index"] == str(index + 1)
        assert row["source_row"] == ""
        expected_location = struct.pack("<d", float(index + 1))
        assert struct.pack("<d", float(row["location"])) == expected_location
        expected_value = struct.unpack_from("<d", oracle, index * 8)[0]
        actual_value = float(row["value"])
        assert struct.pack("<d", actual_value) == struct.pack("<d", expected_value), (
            index
        )

    return {
        "bundle": str(bundle),
        "delimiter": extension,
        "values_checked": values,
        "exact_binary64_values": True,
        "payload_sha256": {
            stem: manifest[f"{stem.title()}-SHA256"] for stem in TABLE_STEMS
        },
    }


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("Usage: python3 verify.py OUTPUT")
    verify_inputs()
    root = owned_root(sys.argv[1])
    receipt_path = root / "independent-python-receipt.json"
    if receipt_path.exists() or receipt_path.is_symlink():
        raise SystemExit(
            "Refusing to overwrite an existing Python verification receipt."
        )

    fixture_checks = [
        verify_fixture_bundle(
            root / f"from-{source}-to-{target}",
            f"roundtrip.{source}",
            target,
        )
        for source in ("csv", "tsv")
        for target in ("csv", "tsv")
    ]

    binary_path = root / "numeric-corpus.f64le"
    r_receipt_path = root / "public-R-receipt.json"
    assert not binary_path.is_symlink() and binary_path.is_file()
    assert not r_receipt_path.is_symlink() and r_receipt_path.is_file()
    binary = binary_path.read_bytes()
    r_receipt = json.loads(r_receipt_path.read_text(encoding="utf-8"))
    corpus_info = r_receipt["numeric_corpus"]
    values = corpus_info["values"]
    assert type(values) is int and values == 20008
    assert corpus_info["seed"] == 81107
    assert corpus_info["rng_kind"] == ["Mersenne-Twister", "Inversion", "Rejection"]
    assert corpus_info["formats"] == ["csv", "tsv"]
    assert corpus_info["binary_file"] == binary_path.name
    assert corpus_info["binary_bytes"] == len(binary) == values * 8
    assert corpus_info["binary_sha256"] == hashlib.sha256(binary).hexdigest()
    corpus_checks = [
        verify_numeric_bundle(
            root / f"numeric-corpus-{extension}", extension, binary, values
        )
        for extension in ("csv", "tsv")
    ]

    result = {
        "profile": "independent-python-stdlib-record-decoder/1",
        "python_version": sys.version,
        "fixture_cross_format_checks": {
            "bundle_count": len(fixture_checks),
            "points_checked": sum(check["points"] for check in fixture_checks),
            "bundles": fixture_checks,
        },
        "numeric_corpus_checks": {
            "seed": corpus_info["seed"],
            "rng_kind": corpus_info["rng_kind"],
            "values": values,
            "formats": corpus_info["formats"],
            "binary_oracle": binary_path.name,
            "binary_oracle_sha256": corpus_info["binary_sha256"],
            "total_values_checked": sum(
                check["values_checked"] for check in corpus_checks
            ),
            "bundles": corpus_checks,
        },
        "scope": "Fixed synthetic inputs only; no instrument or third-party-format qualification.",
    }
    with receipt_path.open("x", encoding="utf-8") as stream:
        json.dump(result, stream, indent=2)
        stream.write("\n")
    print(
        json.dumps(
            {
                "fixture_bundles": len(fixture_checks),
                "fixture_points_checked": sum(
                    check["points"] for check in fixture_checks
                ),
                "numeric_values_checked": sum(
                    check["values_checked"] for check in corpus_checks
                ),
                "all_exact": True,
            }
        )
    )


if __name__ == "__main__":
    main()
