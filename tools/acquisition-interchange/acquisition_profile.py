"""Independent stdlib codecs and oracle for measure-acquisition/1."""

from __future__ import annotations

import csv
import hashlib
import io
import json
import math
import os
import re
import stat
import struct
from pathlib import Path, PurePosixPath, PureWindowsPath
from typing import Any

PROFILE = "measure-acquisition/1"
FORMAT = "measure-acquisition"
VERSION = "1"
DEFAULT_MAX_BYTES = 100 * 1024 * 1024
MAX_ROWS = 1_000_000
MAX_INTEGER = 2_147_483_647
MIN_INTEGER = -2_147_483_647
HEX64 = re.compile(r"[0-9a-f]{16}\Z")
SHA256 = re.compile(r"[0-9a-f]{64}\Z")
INTEGER_TOKEN = re.compile(r"(?:0|-[1-9][0-9]*|[1-9][0-9]*)\Z")
DOUBLE_TOKEN = re.compile(
    r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?\Z"
)

TABLES = {
    "Samples": ("samples", ("row_index", "sample_id")),
    "Injections": ("injections", ("row_index", "injection_id", "sample_id")),
    "Channels": ("channels", ("row_index", "channel_id", "channel_name")),
    "Sources": (
        "sources",
        ("row_index", "source_id", "source_name", "source_sha256"),
    ),
    "Curves": (
        "curves",
        (
            "row_index",
            "curve_id",
            "injection_id",
            "channel_id",
            "source_id",
            "axis_name",
            "axis_unit",
            "value_name",
            "value_unit",
        ),
    ),
    "Points": ("points", ("curve_id", "point_index", "location", "value", "source_row")),
    "Streams": (
        "streams",
        (
            "row_index",
            "stream_id",
            "injection_id",
            "channel_id",
            "kind",
            "source_id",
            "time_unit",
            "mz_unit",
            "value_name",
            "value_unit",
        ),
    ),
    "Scans": (
        "scans",
        (
            "row_index",
            "scan_id",
            "stream_id",
            "scan_index",
            "scan_status",
            "retention_time",
            "polarity",
            "ms_level",
            "spectrum_type",
            "source_scan_id",
            "location_type",
            "value_type",
            "point_count",
        ),
    ),
    "ScanPoints": (
        "scan_points",
        ("scan_id", "point_index", "location", "value", "source_row", "saturated"),
    ),
    "Arrays": (
        "arrays",
        (
            "row_index",
            "array_id",
            "stream_id",
            "dim_names_present",
            "dim_units_present",
            "value_type",
            "point_count",
        ),
    ),
    "ArrayAxes": (
        "array_axes",
        ("array_id", "axis_index", "storage_type", "axis_name", "axis_unit"),
    ),
    "ArrayPoints": (
        "array_points",
        (
            "array_id",
            "point_index",
            "location_1",
            "location_2",
            "value",
            "source_row",
            "saturated",
        ),
    ),
}
MANIFEST_PREFIXES = tuple(TABLES)
FILE_NAMES = {prefix: stem for prefix, (stem, _headers) in TABLES.items()}
MANIFEST_FIELDS = (
    "Format",
    "Version",
    "Delimiter",
    *(f"{prefix}-{suffix}" for prefix in MANIFEST_PREFIXES for suffix in ("Rows", "Columns", "SHA256")),
)
FIXED_INPUT_NAMES = (
    "declaration.json",
    "fixtures.json",
    "wire-schema.json",
    "finite-stress.bin",
)
SOURCE_RECEIPTS = (
    "runtime.json",
    "observations.json",
    "trace-results.json",
    "semantic-rejections.json",
    "run-status.json",
    "progress.json",
)
MUTATION_SOURCE_RECEIPTS = (*SOURCE_RECEIPTS, "independent-python-receipt.json")


class QualificationError(ValueError):
    """Invalid declaration, receipt or native bundle."""


def require(condition: bool, message: str) -> None:
    if not condition:
        raise QualificationError(message)


def exact_equal(actual: Any, expected: Any) -> bool:
    """Compare JSON trees while keeping bool/int and list order distinct."""
    if type(actual) is not type(expected):
        return False
    if type(expected) is dict:
        return actual.keys() == expected.keys() and all(
            exact_equal(actual[key], expected[key]) for key in expected
        )
    if type(expected) is list:
        return len(actual) == len(expected) and all(
            exact_equal(left, right) for left, right in zip(actual, expected, strict=True)
        )
    return actual == expected


def same(actual: Any, expected: Any, label: str) -> None:
    require(exact_equal(actual, expected), f"{label} differs from the frozen declaration")


def keys(value: Any, names: tuple[str, ...] | set[str], label: str) -> None:
    require(type(value) is dict and set(value) == set(names), f"Invalid {label} fields")


def _open_regular(path: Path) -> tuple[int, os.stat_result]:
    try:
        before = path.lstat()
    except OSError as exc:
        raise QualificationError(f"Missing or unreadable file: {path}") from exc
    require(not stat.S_ISLNK(before.st_mode) and stat.S_ISREG(before.st_mode), f"Not a regular nonsymlink file: {path}")
    flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0) | getattr(os, "O_NONBLOCK", 0)
    try:
        descriptor = os.open(path, flags)
    except OSError as exc:
        raise QualificationError(f"File could not be opened safely: {path}") from exc
    after = os.fstat(descriptor)
    if not stat.S_ISREG(after.st_mode):
        os.close(descriptor)
        raise QualificationError(f"Not a regular file after open: {path}")
    return descriptor, after


def read_regular(path: Path, max_bytes: int = DEFAULT_MAX_BYTES) -> bytes:
    require(type(max_bytes) is int and 0 < max_bytes < 2**31, "Invalid byte limit")
    descriptor, _info = _open_regular(Path(path))
    try:
        with os.fdopen(descriptor, "rb") as stream:
            data = stream.read(max_bytes + 1)
    except Exception:
        try:
            os.close(descriptor)
        except OSError:
            pass
        raise
    require(len(data) <= max_bytes, f"File exceeds the actual-byte limit: {path}")
    return data


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def sha256_file(path: Path) -> str:
    return sha256_bytes(read_regular(path))


def _json_pairs(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise QualificationError(f"Duplicate JSON object key: {key}")
        result[key] = value
    return result


def _reject_json_float(token: str) -> Any:
    raise QualificationError(f"JSON floating-point literals are not allowed: {token}")


def _reject_json_constant(token: str) -> Any:
    raise QualificationError(f"Nonstandard JSON numeric token: {token}")


def _validate_json_strings(value: Any, label: str) -> None:
    if type(value) is str:
        _strict_string(value, label)
    elif type(value) is list:
        for index, item in enumerate(value):
            _validate_json_strings(item, f"{label}[{index}]")
    elif type(value) is dict:
        for key, item in value.items():
            _strict_string(key, f"{label} key")
            _validate_json_strings(item, f"{label}.{key}")


def loads_json(data: bytes, label: str) -> Any:
    require(b"\x00" not in data, f"JSON contains NUL: {label}")
    try:
        text = data.decode("utf-8", errors="strict")
        value = json.loads(
            text,
            object_pairs_hook=_json_pairs,
            parse_float=_reject_json_float,
            parse_constant=_reject_json_constant,
        )
    except (UnicodeError, json.JSONDecodeError) as exc:
        raise QualificationError(f"Invalid strict UTF-8 JSON: {label}") from exc
    _validate_json_strings(value, label)
    return value


def read_json(path: Path, max_bytes: int = DEFAULT_MAX_BYTES) -> Any:
    return loads_json(read_regular(path, max_bytes), str(path))


def write_json_exclusive(path: Path, value: Any) -> None:
    payload = (
        json.dumps(value, ensure_ascii=False, allow_nan=False, indent=2, separators=(",", ": "))
        + "\n"
    ).encode("utf-8", errors="strict")
    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL | getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(path, flags, 0o666)
    except FileExistsError as exc:
        raise QualificationError(f"Refusing to overwrite receipt: {path}") from exc
    with os.fdopen(descriptor, "wb") as stream:
        stream.write(payload)
        stream.flush()
        os.fsync(stream.fileno())


def _strict_string(value: Any, label: str, allow_empty: bool = True) -> str:
    require(type(value) is str and (allow_empty or value != ""), f"{label} must be text")
    try:
        value.encode("utf-8", errors="strict")
    except UnicodeError as exc:
        raise QualificationError(f"{label} is not valid UTF-8") from exc
    require("\x00" not in value, f"{label} contains NUL")
    return value


def _strict_index(value: Any, label: str, positive: bool = False) -> int:
    require(type(value) is int, f"{label} must be a JSON integer")
    require(value <= MAX_INTEGER and value >= MIN_INTEGER, f"{label} is outside R integer range")
    if positive:
        require(value > 0, f"{label} must be positive")
    return value


def _decode_atom(atom: Any, label: str) -> tuple[str, float | None]:
    require(type(atom) is dict, f"{label} must be a typed double atom")
    if set(atom) == {"hex"}:
        token = atom["hex"]
        require(type(token) is str and HEX64.fullmatch(token) is not None, f"{label} has invalid binary64 hex")
        value = struct.unpack(">d", bytes.fromhex(token))[0]
        require(math.isfinite(value), f"{label} finite bits encode a nonfinite value")
        return "hex", value
    if set(atom) == {"state"}:
        state = atom["state"]
        require(state in {"na", "nan", "positive_infinity", "negative_infinity"}, f"{label} has invalid state")
        value = {
            "na": None,
            "nan": math.nan,
            "positive_infinity": math.inf,
            "negative_infinity": -math.inf,
        }[state]
        return state, value
    raise QualificationError(f"{label} must have exactly hex or state")


def _atom_from_float(value: float) -> dict[str, str]:
    return {"hex": struct.pack(">d", value).hex()}


def _state_for_token(token: str) -> tuple[str, float | None]:
    states = {
        "NA": ("na", None),
        "NaN": ("nan", math.nan),
        "Inf": ("positive_infinity", math.inf),
        "-Inf": ("negative_infinity", -math.inf),
    }
    if token in states:
        return states[token]
    if token == "-0":
        return ("hex", -0.0)
    require(DOUBLE_TOKEN.fullmatch(token) is not None, f"Invalid double token: {token}")
    value = float(token)
    require(math.isfinite(value), f"Double token outside finite range: {token}")
    significand = re.split(r"[eE]", token, maxsplit=1)[0]
    require(not (value == 0.0 and re.search(r"[1-9]", significand)), f"Nonzero token underflows: {token}")
    return ("hex", value)


def _float_token(value: float) -> str:
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "Inf" if value > 0 else "-Inf"
    if value == 0.0 and math.copysign(1.0, value) < 0:
        return "-0"
    return format(value, ".17g")


def _double_atom_token(atom: Any, label: str, scalar_record: bool = False) -> str:
    kind, value = _decode_atom(atom, label)
    if kind == "na":
        return "" if scalar_record else "NA"
    require(value is not None, f"Invalid {label}")
    if scalar_record:
        require(math.isfinite(value), f"Scalar record {label} must be finite or NA")
        if value == 0.0:
            value = 0.0
    return _float_token(value)


def decode_vector(vector: Any, label: str, corpus_path: Path | None = None) -> tuple[str, list[Any]]:
    require(type(vector) is dict, f"{label} must be a typed vector")
    storage = vector.get("storage")
    require(storage in {"integer", "double", "logical"}, f"{label} has unsupported storage")
    if set(vector) == {"storage", "corpus"}:
        require(storage == "double" and vector["corpus"] == "finite-stress.bin", f"{label} has unsupported corpus source")
        require(corpus_path is not None, f"{label} needs the frozen finite corpus")
        raw = read_regular(corpus_path, 65664)
        require(len(raw) == 65664, "Finite stress corpus has wrong byte count")
        values = [struct.unpack_from(">d", raw, offset)[0] for offset in range(0, len(raw), 8)]
        require(all(math.isfinite(value) for value in values), "Finite stress corpus contains nonfinite values")
        return storage, [("hex", value) for value in values]
    keys(vector, ("storage", "data"), label)
    data = vector["data"]
    require(type(data) is list and len(data) <= MAX_ROWS, f"{label}.data must be a bounded array")
    if storage == "integer":
        values = []
        for index, value in enumerate(data):
            if value is None:
                values.append(None)
            else:
                values.append(_strict_index(value, f"{label}[{index}]"))
        return storage, values
    if storage == "logical":
        require(all(value is None or type(value) is bool for value in data), f"{label} has invalid logical values")
        return storage, data.copy()
    decoded = []
    for index, atom in enumerate(data):
        kind, value = _decode_atom(atom, f"{label}[{index}]")
        decoded.append(None if kind == "na" else value)
        if kind == "na":
            decoded[-1] = ("na", None)
        elif kind == "nan":
            decoded[-1] = ("nan", math.nan)
        elif kind == "positive_infinity":
            decoded[-1] = ("positive_infinity", math.inf)
        elif kind == "negative_infinity":
            decoded[-1] = ("negative_infinity", -math.inf)
        else:
            decoded[-1] = ("hex", value)
    return storage, decoded


def canonical_vector(vector: Any, label: str, corpus_path: Path | None = None) -> dict[str, Any]:
    storage, values = decode_vector(vector, label, corpus_path)
    if storage == "integer" or storage == "logical":
        data = values
    else:
        data = []
        for state, value in values:
            if state == "hex":
                data.append(_atom_from_float(value))
            else:
                data.append({"state": state})
    return {"storage": storage, "data": data}


def _vector_tokens(vector: Any, label: str, corpus_path: Path | None, scalar_record: bool = False) -> tuple[str, list[str]]:
    storage, values = decode_vector(vector, label, corpus_path)
    if storage == "integer":
        return storage, ["NA" if value is None else str(value) for value in values]
    if storage == "logical":
        raise QualificationError(f"Numeric wire column cannot use logical storage: {label}")
    tokens = []
    for state, value in values:
        if state == "na":
            if scalar_record:
                tokens.append("")
            else:
                tokens.append("NA")
            continue
        require(value is not None, f"Invalid {label} state")
        if scalar_record:
            require(state == "hex" and math.isfinite(value), f"Scalar record {label} must be finite or NA")
            if value == 0.0:
                value = 0.0
        tokens.append(_float_token(value))
    return storage, tokens


def _text(value: Any, label: str) -> str:
    if value is None:
        return ""
    text = _strict_string(value, label)
    require(not _r_blank(text), f"{label} must not be blank")
    return text


def _row_from_values(headers: tuple[str, ...], values: dict[str, str]) -> list[str]:
    return [values[field] for field in headers]


def _expected_tables_for_object(fixtures: dict[str, Any], object_id: str, corpus_path: Path) -> dict[str, list[list[str]]]:
    object_rows = {item["id"]: item for item in fixtures["objects"]}
    require(object_id in object_rows, f"Unknown declared acquisition object: {object_id}")
    obj = object_rows[object_id]
    record_templates = fixtures["record_templates"]
    record_id = obj["record_template"]
    require(record_id in record_templates, f"Unknown record template: {record_id}")
    record = record_templates[record_id]

    tables: dict[str, list[list[str]]] = {prefix: [] for prefix in MANIFEST_PREFIXES}

    for index, row in enumerate(record["samples"], 1):
        tables["Samples"].append([str(index), _text(row["sample_id"], "sample_id")])
    for index, row in enumerate(record["injections"], 1):
        tables["Injections"].append([str(index), _text(row["injection_id"], "injection_id"), _text(row["sample_id"], "injection.sample_id")])
    for index, row in enumerate(record["channels"], 1):
        tables["Channels"].append([str(index), _text(row["channel_id"], "channel_id"), _text(row["channel_name"], "channel_name")])
    for index, row in enumerate(record["sources"], 1):
        tables["Sources"].append([str(index), _text(row["source_id"], "source_id"), _text(row["source_name"], "source_name"), _text(row["source_sha256"], "source_sha256")])

    for index, row in enumerate(record["curves"], 1):
        tables["Curves"].append(
            [
                str(index), _text(row["curve_id"], "curve_id"), _text(row["injection_id"], "curve.injection_id"),
                _text(row["channel_id"], "curve.channel_id"), _text(row["source_id"], "curve.source_id"),
                _text(row["axis_name"], "curve.axis_name"), _text(row["axis_unit"], "curve.axis_unit"),
                _text(row["value_name"], "curve.value_name"), _text(row["value_unit"], "curve.value_unit"),
            ]
        )
        loc_storage, locations = _vector_tokens(row["location"], "curve.location", corpus_path, scalar_record=True)
        val_storage, values = _vector_tokens(row["value"], "curve.value", corpus_path, scalar_record=True)
        require(loc_storage == "double" and val_storage == "double", "Scalar record curve vectors must be double")
        require(len(locations) == len(values) == len(row["source_rows"]), "Scalar record curve point vectors differ in length")
        for point_index, (location, value, source_row) in enumerate(zip(locations, values, row["source_rows"], strict=True), 1):
            tables["Points"].append([_text(row["curve_id"], "curve_id"), str(point_index), location, value, "" if source_row is None else str(_strict_index(source_row, "curve.source_rows", positive=True))])

    for index, row in enumerate(obj["streams"], 1):
        tables["Streams"].append(
            [
                str(index), _text(row["stream_id"], "stream_id"), _text(row["injection_id"], "stream.injection_id"),
                _text(row["channel_id"], "stream.channel_id"), _text(row["kind"], "stream.kind"),
                _text(row["source_id"], "stream.source_id"), _text(row["time_unit"], "stream.time_unit"),
                _text(row["mz_unit"], "stream.mz_unit"), _text(row["value_name"], "stream.value_name"),
                _text(row["value_unit"], "stream.value_unit"),
            ]
        )

    for index, row in enumerate(obj["scans"], 1):
        loc_storage, locations = _vector_tokens(row["measures"]["location"], "scan.location", corpus_path)
        val_storage, values = _vector_tokens(row["measures"]["value"], "scan.value", corpus_path)
        require(loc_storage in {"integer", "double"} and val_storage in {"integer", "double"}, "Scans require numeric storage")
        count = len(locations)
        require(len(values) == count == len(row["source_rows"]) == len(row["saturated"]), f"Scan vectors differ in length for {row['scan_id']}")
        retention = _double_atom_token(row["retention_time"], "retention_time")
        tables["Scans"].append(
            [
                str(index), _text(row["scan_id"], "scan_id"), _text(row["stream_id"], "scan.stream_id"),
                str(_strict_index(row["scan_index"], "scan_index", positive=True)), _text(row["scan_status"], "scan_status"),
                retention, _text(row["polarity"], "scan.polarity"),
                "" if row["ms_level"] is None else str(_strict_index(row["ms_level"], "scan.ms_level", positive=True)),
                _text(row["spectrum_type"], "scan.spectrum_type"), _text(row["source_scan_id"], "source_scan_id"),
                loc_storage, val_storage, str(count),
            ]
        )
        for point_index, (location, value, source_row, saturated) in enumerate(zip(locations, values, row["source_rows"], row["saturated"], strict=True), 1):
            source_token = "" if source_row is None else str(_strict_index(source_row, "scan.source_rows", positive=True))
            saturated_token = "" if saturated is None else ("1" if saturated is True else "0")
            tables["ScanPoints"].append([_text(row["scan_id"], "scan_id"), str(point_index), location, value, source_token, saturated_token])

    for index, row in enumerate(obj["arrays"], 1):
        measures = row["measures"]
        axis_vectors = [measures["location_1"], measures["location_2"]]
        axis_data = [_vector_tokens(vector, f"array.{name}", corpus_path) for name, vector in zip(("location_1", "location_2"), axis_vectors, strict=True)]
        value_storage, values = _vector_tokens(measures["value"], "array.value", corpus_path)
        require(all(storage in {"integer", "double"} for storage, _ in axis_data) and value_storage in {"integer", "double"}, "Arrays require numeric storage")
        count = len(values)
        require(all(len(data) == count for _storage, data in axis_data), f"Array coordinate lengths differ for {row['array_id']}")
        require(len(row["source_rows"]) == count and len(row["saturated"]) == count, f"Array references/masks differ for {row['array_id']}")
        dim_names = measures["dim_names"]
        dim_units = measures["dim_units"]
        require(dim_names is None or type(dim_names) is list and len(dim_names) == 2, "Array dim_names must be null or a two-entry list")
        require(dim_units is None or type(dim_units) is list and len(dim_units) == 2, "Array dim_units must be null or a two-entry list")
        tables["Arrays"].append([
            str(index), _text(row["array_id"], "array_id"), _text(row["stream_id"], "array.stream_id"),
            "0" if dim_names is None else "1", "0" if dim_units is None else "1", value_storage, str(count),
        ])
        for axis_index in range(2):
            tables["ArrayAxes"].append([
                _text(row["array_id"], "array_id"), str(axis_index + 1), axis_data[axis_index][0],
                "" if dim_names is None else _text(dim_names[axis_index], "dim_name"),
                "" if dim_units is None else _text(dim_units[axis_index], "dim_unit"),
            ])
        for point_index, (loc1, loc2, value, source_row, saturated) in enumerate(zip(axis_data[0][1], axis_data[1][1], values, row["source_rows"], row["saturated"], strict=True), 1):
            source_token = "" if source_row is None else str(_strict_index(source_row, "array.source_rows", positive=True))
            saturated_token = "" if saturated is None else ("1" if saturated is True else "0")
            tables["ArrayPoints"].append([_text(row["array_id"], "array_id"), str(point_index), loc1, loc2, value, source_token, saturated_token])

    for prefix, (_stem, headers) in TABLES.items():
        for row in tables[prefix]:
            require(len(row) == len(headers), f"Internal oracle row width mismatch: {prefix}")
    return tables


def expected_observations(fixtures: dict[str, Any], object_id: str, corpus_path: Path) -> dict[str, Any]:
    object_rows = {item["id"]: item for item in fixtures["objects"]}
    require(object_id in object_rows, f"Unknown object id: {object_id}")
    source = object_rows[object_id]
    record_source = fixtures["record_templates"][source["record_template"]]
    record = {
        "samples": record_source["samples"],
        "injections": record_source["injections"],
        "channels": record_source["channels"],
        "sources": record_source["sources"],
        "curves": [],
    }
    for row in record_source["curves"]:
        record["curves"].append({
            key: row[key]
            for key in (
                "curve_id", "injection_id", "channel_id", "source_id", "axis_name", "axis_unit",
                "value_name", "value_unit", "source_rows",
            )
        } | {
            "location": canonical_vector(row["location"], "curve.location", corpus_path),
            "value": canonical_vector(row["value"], "curve.value", corpus_path),
        })
    record = {key: list(value) for key, value in record.items()}
    for row in record["curves"]:
        for field in ("location", "value"):
            vector = row[field]
            for atom in vector["data"]:
                if "state" not in atom and struct.unpack(">d", bytes.fromhex(atom["hex"]))[0] == 0.0:
                    atom["hex"] = "0000000000000000"
    streams = source["streams"]
    scans = []
    for row in source["scans"]:
        scans.append({
            **{key: row[key] for key in ("scan_id", "stream_id", "scan_index", "scan_status", "retention_time", "polarity", "ms_level", "spectrum_type", "source_scan_id", "source_rows", "saturated")},
            "measures": {
                key: canonical_vector(value, f"scan.{key}", corpus_path)
                for key, value in row["measures"].items()
            },
        })
    arrays = []
    for row in source["arrays"]:
        arrays.append({
            "array_id": row["array_id"],
            "stream_id": row["stream_id"],
            "measures": {
                key: canonical_vector(value, f"array.{key}", corpus_path) if key in {"location_1", "location_2", "value"} else value
                for key, value in row["measures"].items()
            },
            "source_rows": row["source_rows"],
            "saturated": row["saturated"],
        })
    return {"id": object_id, "record": record, "streams": streams, "scans": scans, "arrays": arrays}


def expected_all_objects(fixtures: dict[str, Any], declaration: dict[str, Any], corpus_path: Path) -> dict[str, dict[str, Any]]:
    ids = [row["id"] for row in fixtures["objects"]]
    same(ids, declaration["object_ids"], "Object declaration order")
    return {object_id: expected_observations(fixtures, object_id, corpus_path) for object_id in ids}


def load_inputs(tool_dir: Path | None = None) -> tuple[dict[str, Any], dict[str, Any], dict[str, str]]:
    root = (tool_dir or Path(__file__).resolve().parent).resolve(strict=True)
    manifest = read_json(root / "input-manifest.json", 128 * 1024)
    keys(manifest, ("profile", "files"), "input manifest")
    same(manifest["profile"], PROFILE, "Input profile")
    files = manifest["files"]
    keys(files, FIXED_INPUT_NAMES, "input file map")
    hashes: dict[str, str] = {}
    for name in FIXED_INPUT_NAMES:
        item = files[name]
        keys(item, ("sha256", "size_bytes"), f"input manifest {name}")
        digest = item["sha256"]
        size = item["size_bytes"]
        require(type(digest) is str and SHA256.fullmatch(digest) is not None, f"Invalid frozen hash for {name}")
        require(type(size) is int and 0 <= size < 2**31, f"Invalid frozen byte size for {name}")
        data = read_regular(root / name, size)
        same(len(data), size, f"Frozen size {name}")
        same(sha256_bytes(data), digest, f"Frozen SHA256 {name}")
        hashes[name] = digest
    declaration = loads_json(read_regular(root / "declaration.json", 2 * 1024 * 1024), "declaration.json")
    fixtures = loads_json(read_regular(root / "fixtures.json", 16 * 1024 * 1024), "fixtures.json")
    wire = loads_json(read_regular(root / "wire-schema.json", 128 * 1024), "wire-schema.json")
    keys(
        declaration,
        (
            "profile", "contract_sha256", "wire_schema_sha256", "object_ids", "forward", "controls", "invalid",
            "cross_format", "semantic_rejection_ids", "trace_ids", "finite_stress", "payload_table_count",
            "declaration_before_qualification_execution", "scope",
        ),
        "declaration",
    )
    same(declaration["profile"], PROFILE, "Declaration profile")
    same(declaration["wire_schema_sha256"], hashes["wire-schema.json"], "Wire schema hash")
    keys(wire, ("format", "version", "tables", "metadata_order_field", "mask_tokens", "contract_sha256"), "wire schema")
    same(wire["format"], FORMAT, "Wire format")
    same(wire["version"], 1, "Wire version")
    same(wire["contract_sha256"], declaration["contract_sha256"], "Accepted contract hash")
    same(wire["metadata_order_field"], "row_index", "Metadata order field")
    same(wire["mask_tokens"], {"true": "1", "false": "0", "unknown": ""}, "Mask tokens")
    schema_headers = {name: list(headers) for name, (_stem, headers) in TABLES.items()}
    same(wire["tables"], schema_headers, "Wire headers")
    keys(fixtures, ("profile", "vector_grammar", "record_templates", "objects", "traces", "semantic_rejections"), "fixtures")
    same(fixtures["profile"], PROFILE, "Fixture profile")
    object_ids = [item["id"] for item in fixtures["objects"]]
    same(object_ids, declaration["object_ids"], "Object IDs")
    same([item["id"] for item in fixtures["traces"]], declaration["trace_ids"], "Trace IDs")
    same([item["id"] for item in fixtures["semantic_rejections"]], declaration["semantic_rejection_ids"], "Trace rejection IDs")
    stress = declaration["finite_stress"]
    keys(stress, ("file", "storage", "byte_order", "values", "bytes", "sha256"), "finite stress declaration")
    same(stress, {
        "file": "finite-stress.bin",
        "storage": "double",
        "byte_order": "big",
        "values": 8208,
        "bytes": 65664,
        "sha256": hashes["finite-stress.bin"],
    }, "Finite stress corpus declaration")
    same(declaration["payload_table_count"], 12, "Payload table count")
    same(declaration["declaration_before_qualification_execution"], True, "Declaration timing")
    require(type(declaration["scope"]) is str and declaration["scope"], "Missing scope boundary")
    require(len(object_ids) == len(set(object_ids)), "Duplicate object IDs")
    require(len(fixtures["record_templates"]) > 0, "No scalar record templates")
    require(len(declaration["forward"]) == len(object_ids) * 2, "Forward bundle count does not cover every object and delimiter")
    for descriptor in declaration["forward"]:
        keys(descriptor, ("id", "object_id", "path", "delimiter"), "forward descriptor")
        require(descriptor["object_id"] in object_ids, "Unknown forward object")
        require(descriptor["delimiter"] in {"csv", "tsv"}, "Invalid forward delimiter")
        same(descriptor["path"], f"bundles/{descriptor['id']}", "Forward path")
    for descriptor in declaration["controls"]:
        allowed = {"id", "kind", "object_id", "path", "delimiter", "source_case_id"}
        require(type(descriptor) is dict and set(descriptor) <= allowed and {"id", "kind", "object_id", "path", "delimiter"} <= set(descriptor), "Invalid control descriptor")
        require(descriptor["kind"] in {"reverse", "reorder"}, "Invalid control kind")
        require(descriptor["object_id"] in object_ids and descriptor["delimiter"] in {"csv", "tsv"}, "Invalid control target")
        same(descriptor["path"], f"controls/{descriptor['id']}", "Control path")
    for descriptor in declaration["invalid"]:
        keys(descriptor, ("id", "source_case_id", "path", "mutation"), "invalid descriptor")
        same(descriptor["path"], f"mutants/{descriptor['id']}", "Invalid bundle path")
        same(descriptor["mutation"], descriptor["id"], "Mutation selector")
    for descriptor in declaration["cross_format"]:
        require(type(descriptor) is dict and type(descriptor.get("id")) is str, "Invalid cross-format descriptor")
    corpus = read_regular(root / "finite-stress.bin", 65664)
    require(len(corpus) == 8208 * 8, "Finite stress corpus has the wrong number of bytes")
    require(all(math.isfinite(value) for (value,) in struct.iter_unpack(">d", corpus)), "Finite stress corpus contains nonfinite data")
    corpus_path = root / "finite-stress.bin"
    for object_id in object_ids:
        _expected_tables_for_object(fixtures, object_id, corpus_path)
    return fixtures, declaration, hashes


def _parse_uint(token: str, label: str, maximum: int = MAX_INTEGER) -> int:
    require(type(token) is str and re.fullmatch(r"(?:0|[1-9][0-9]*)", token) is not None, f"Invalid nonnegative integer in {label}")
    number = int(token)
    require(number <= maximum, f"Integer is outside the supported range in {label}")
    return number


def _parse_int(token: str, label: str, missing: bool = False) -> int | None:
    if missing and token == "NA":
        return None
    require(type(token) is str and INTEGER_TOKEN.fullmatch(token) is not None, f"Invalid integer token in {label}")
    number = int(token)
    require(MIN_INTEGER <= number <= MAX_INTEGER, f"Integer is outside R range in {label}")
    return number


def _parse_double(token: str, label: str, missing: bool = False) -> float | None:
    if missing and token in {"", "NA"}:
        return None
    _kind, value = _state_for_token(token)
    return value


def _semantic_numeric_token(
    token: str,
    storage: str,
    label: str,
    *,
    missing_empty: bool = False,
    normalize_positive_zero: bool = False,
) -> tuple[Any, ...]:
    if storage == "integer":
        value = _parse_int(token, label, missing=True)
        return ("integer", "na") if value is None else ("integer", value)
    require(storage == "double", f"Invalid numeric storage in {label}")
    if missing_empty and token == "":
        return ("double", "na")
    state, value = _state_for_token(token)
    if state != "hex":
        return ("double", state)
    require(value is not None, f"Missing finite numeric value in {label}")
    if normalize_positive_zero and value == 0.0:
        value = 0.0
    return ("double", "hex", struct.pack(">d", value).hex())


def _r_blank(value: str) -> bool:
    # Mirrors the existing nD codec's explicit R PCRE [\h\v] character set.
    # Python str.isspace() differs for U+001C and U+180E.
    whitespace = set("\t \u00a0\u1680\u180e") | set(
        "\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a"
        "\u202f\u205f\u3000\n\v\f\r\u0085\u2028\u2029"
    )
    return not value or all(character in whitespace for character in value)


def _check_text(value: str, label: str, nonblank: bool = False) -> None:
    _strict_string(value, label)
    if nonblank or value != "":
        require(not _r_blank(value), f"{label} must not be blank")


def _parse_manifest(data: bytes) -> dict[str, str]:
    require(data.endswith(b"\n"), "Manifest must end with a newline")
    require(b"\r" not in data and b"\x00" not in data, "Manifest contains unsupported control bytes")
    try:
        text = data.decode("utf-8", errors="strict")
    except UnicodeError as exc:
        raise QualificationError("Manifest is not valid UTF-8") from exc
    fields: dict[str, str] = {}
    # DCF records are physical LF-terminated lines. str.splitlines() also
    # splits VT, FF, NEL, Unicode separators, and other characters the schema
    # does not permit as record boundaries.
    for line in text[:-1].split("\n"):
        require(line != "", "Manifest contains a blank record")
        match = re.fullmatch(r"([A-Za-z][A-Za-z0-9-]*): (.*)", line)
        require(match is not None, "Malformed DCF field line")
        key, value = match.groups()
        require(key not in fields, f"Manifest contains duplicate field {key}")
        fields[key] = value
    same(set(fields), set(MANIFEST_FIELDS), "Manifest fields")
    same(fields["Format"], FORMAT, "Manifest format")
    same(fields["Version"], VERSION, "Manifest version")
    require(fields["Delimiter"] in {"csv", "tsv"}, "Invalid manifest delimiter")
    for prefix in MANIFEST_PREFIXES:
        _parse_uint(fields[f"{prefix}-Rows"], f"{prefix}-Rows", MAX_ROWS)
        columns = _parse_uint(fields[f"{prefix}-Columns"], f"{prefix}-Columns", 1024)
        same(columns, len(TABLES[prefix][1]), f"{prefix} columns")
        digest = fields[f"{prefix}-SHA256"]
        require(SHA256.fullmatch(digest) is not None, f"Invalid {prefix} hash syntax")
    return fields


def _parse_csv(data: bytes, delimiter: str, expected_header: tuple[str, ...], label: str) -> list[list[str]]:
    require(b"\x00" not in data, f"{label} contains NUL")
    try:
        text = data.decode("utf-8", errors="strict")
        reader = csv.reader(io.StringIO(text, newline=""), delimiter=delimiter, strict=True)
        rows = list(reader)
    except (UnicodeError, csv.Error) as exc:
        raise QualificationError(f"Malformed UTF-8 {label}") from exc
    require(rows and tuple(rows[0]) == expected_header, f"Wrong {label} header")
    for row in rows:
        require(len(row) == len(expected_header), f"Wrong column count in {label}")
        for cell in row:
            _strict_string(cell, label)
    require(len(rows) - 1 <= MAX_ROWS, f"Too many rows in {label}")
    return rows


def _check_unique(values: list[str], label: str, nonblank: bool = True) -> None:
    if nonblank:
        for value in values:
            _check_text(value, label, nonblank=True)
    require(len(values) == len(set(values)), f"Duplicate {label}")


def _check_contiguous(values: list[str], label: str) -> None:
    parsed = [_parse_uint(value, label, MAX_ROWS) for value in values]
    same(sorted(parsed), list(range(1, len(parsed) + 1)), f"Contiguous {label}")


def _check_point_groups(rows: list[list[str]], owner_column: int, index_column: int, owners: dict[str, int], label: str) -> dict[str, list[tuple[int, list[str]]]]:
    grouped: dict[str, list[tuple[int, list[str]]]] = {owner: [] for owner in owners}
    for row in rows:
        owner = row[owner_column]
        require(owner in owners, f"Unknown {label} owner {owner}")
        point_index = _parse_uint(row[index_column], f"{label}.point_index", MAX_ROWS)
        require(point_index > 0, f"{label}.point_index must be positive")
        grouped[owner].append((point_index, row))
    for owner, points in grouped.items():
        indexes = [index for index, _row in points]
        same(sorted(indexes), list(range(1, len(indexes) + 1)), f"{label} point order for {owner}")
    return grouped


def _validate_bundle_tables(manifest: dict[str, str], tables: dict[str, list[list[str]]]) -> None:
    delimiter = manifest["Delimiter"]
    del delimiter  # Delimiter is validated while each table is decoded.
    header_rows = {prefix: rows[1:] for prefix, rows in tables.items()}
    for prefix, rows in header_rows.items():
        same(len(rows), _parse_uint(manifest[f"{prefix}-Rows"], f"{prefix}-Rows", MAX_ROWS), f"{prefix} row count")
        same(len(rows[0]) if rows else len(TABLES[prefix][1]), _parse_uint(manifest[f"{prefix}-Columns"], f"{prefix}-Columns", 1024), f"{prefix} column count")

    def col(prefix: str, name: str) -> list[str]:
        i = TABLES[prefix][1].index(name)
        return [row[i] for row in header_rows[prefix]]

    metadata_prefixes = ("Samples", "Injections", "Channels", "Sources", "Curves", "Streams", "Scans", "Arrays")
    for prefix in metadata_prefixes:
        _check_contiguous(col(prefix, "row_index"), f"{prefix}.row_index")

    samples = col("Samples", "sample_id")
    injections = col("Injections", "injection_id")
    channels = col("Channels", "channel_id")
    sources = col("Sources", "source_id")
    curves = col("Curves", "curve_id")
    streams = col("Streams", "stream_id")
    scans = col("Scans", "scan_id")
    arrays = col("Arrays", "array_id")
    for values, label in (
        (samples, "sample_id"), (injections, "injection_id"), (channels, "channel_id"),
        (sources, "source_id"), (curves, "curve_id"), (streams, "stream_id"),
        (scans, "scan_id"), (arrays, "array_id"),
    ):
        _check_unique(values, label)

    sample_set, injection_set, channel_set, source_set = set(samples), set(injections), set(channels), set(sources)
    curve_by_id: dict[str, list[str]] = {}
    for row in header_rows["Injections"]:
        require(row[2] in sample_set, f"Unknown sample in injection {row[1]}")
    for row in header_rows["Sources"]:
        _check_text(row[2], "source_name")
        if row[3] != "":
            require(SHA256.fullmatch(row[3]) is not None, "Invalid source SHA256")
    for row in header_rows["Channels"]:
        _check_text(row[2], "channel_name")
    for row in header_rows["Curves"]:
        require(row[2] in injection_set and row[3] in channel_set, f"Unknown scalar curve reference {row[1]}")
        require(row[4] == "" or row[4] in source_set, f"Unknown scalar curve source {row[1]}")
        curve_by_id[row[1]] = row
        for field in row[5:9]:
            _check_text(field, "curve metadata")
    for row in header_rows["Streams"]:
        require(row[2] in injection_set and row[3] in channel_set, f"Unknown stream record reference {row[1]}")
        require(row[5] == "" or row[5] in source_set, f"Unknown stream source {row[1]}")
        require(row[4] in {"ms", "nd"}, f"Invalid stream kind {row[1]}")
        if row[4] == "nd":
            require(row[6] == "" and row[7] == "", f"ND stream {row[1]} declares MS units")
        for field in row[6:10]:
            _check_text(field, "stream metadata")
    stream_by_id = {row[1]: row for row in header_rows["Streams"]}
    scan_by_id: dict[str, list[str]] = {}
    scan_indices: dict[str, list[int]] = {}
    for row in header_rows["Scans"]:
        require(row[2] in stream_by_id and stream_by_id[row[2]][4] == "ms", f"Scan references a non-MS stream {row[1]}")
        scan_index = _parse_uint(row[3], "Scans.scan_index", MAX_INTEGER)
        require(scan_index > 0, f"Invalid scan_index for {row[1]}")
        scan_indices.setdefault(row[2], []).append(scan_index)
        require(row[4] in {"acquired", "missing"}, f"Invalid scan status {row[1]}")
        retention = _parse_double(row[5], "Scans.retention_time")
        require(retention is None or math.isfinite(retention), f"Invalid retention time for {row[1]}")
        require(row[6] in {"", "positive", "negative"}, f"Invalid polarity for {row[1]}")
        if row[7] != "":
            level = _parse_int(row[7], "Scans.ms_level")
            require(level is not None and level > 0, f"Invalid MS level for {row[1]}")
        require(row[8] in {"", "centroid", "profile"}, f"Invalid spectrum type for {row[1]}")
        if row[9] != "":
            _check_text(row[9], "source_scan_id", nonblank=True)
        require(row[10] in {"integer", "double"} and row[11] in {"integer", "double"}, f"Invalid scan vector descriptor for {row[1]}")
        count = _parse_uint(row[12], "Scans.point_count", MAX_ROWS)
        require(row[4] != "missing" or count == 0, f"Missing scan {row[1]} has points")
        scan_by_id[row[1]] = row
    for stream_id, indexes in scan_indices.items():
        require(len(indexes) == len(set(indexes)), f"Duplicate scan_index within {stream_id}")

    array_by_id: dict[str, list[str]] = {}
    for row in header_rows["Arrays"]:
        require(row[2] in stream_by_id and stream_by_id[row[2]][4] == "nd", f"Array references a non-ND stream {row[1]}")
        require(row[3] in {"0", "1"} and row[4] in {"0", "1"}, f"Invalid array dimension presence flag {row[1]}")
        require(row[5] in {"integer", "double"}, f"Invalid array value type {row[1]}")
        _parse_uint(row[6], "Arrays.point_count", MAX_ROWS)
        array_by_id[row[1]] = row

    curve_groups = _check_point_groups(header_rows["Points"], 0, 1, {key: index for index, key in enumerate(curve_by_id)}, "Points")
    for curve_id, group in curve_groups.items():
        require(bool(group), f"Scalar curve {curve_id} has no points")
        locations: list[float] = []
        for _index, row in sorted(group, key=lambda item: item[0]):
            location = _parse_double(row[2], "Points.location")
            value = _parse_double(row[3], "Points.value", missing=True)
            require(location is not None and math.isfinite(location), f"Invalid scalar location in {curve_id}")
            require(value is None or math.isfinite(value), f"Invalid scalar value in {curve_id}")
            require(row[4] == "" or (_parse_int(row[4], "Points.source_row") is not None and _parse_int(row[4], "Points.source_row") > 0), f"Invalid scalar source row in {curve_id}")
            locations.append(location)
        if len(locations) > 1:
            increasing = all(a < b for a, b in zip(locations, locations[1:]))
            decreasing = all(a > b for a, b in zip(locations, locations[1:]))
            require(increasing or decreasing, f"Scalar locations are not strictly monotonic in {curve_id}")

    scan_point_groups = _check_point_groups(header_rows["ScanPoints"], 0, 1, {key: index for index, key in enumerate(scan_by_id)}, "ScanPoints")
    for scan_id, group in scan_point_groups.items():
        scan = scan_by_id[scan_id]
        same(len(group), _parse_uint(scan[12], f"{scan_id}.point_count", MAX_ROWS), f"Scan point count {scan_id}")
        for _index, row in group:
            _parse_typed_numeric(row[2], scan[10], f"{scan_id}.location")
            _parse_typed_numeric(row[3], scan[11], f"{scan_id}.value")
            _parse_optional_positive(row[4], f"{scan_id}.source_row")
            require(row[5] in {"", "0", "1"}, f"Invalid saturation token for {scan_id}")

    axes_by_array: dict[str, list[tuple[int, list[str]]]] = {key: [] for key in array_by_id}
    for row in header_rows["ArrayAxes"]:
        require(row[0] in array_by_id, f"Unknown array axis owner {row[0]}")
        axis_index = _parse_uint(row[1], "ArrayAxes.axis_index", 2)
        require(axis_index in {1, 2}, f"Invalid array axis index for {row[0]}")
        require(row[2] in {"integer", "double"}, f"Invalid array coordinate storage for {row[0]}")
        axes_by_array[row[0]].append((axis_index, row))
        _check_text(row[3], "array axis name")
        _check_text(row[4], "array axis unit")
    for array_id, axes in axes_by_array.items():
        same(sorted(index for index, _row in axes), [1, 2], f"Array axes for {array_id}")
        axes.sort(key=lambda item: item[0])
        metadata = array_by_id[array_id]
        for axis_index, row in axes:
            name = row[3]
            unit = row[4]
            if metadata[3] == "0":
                same(name, "", f"Absent array dim_names {array_id}")
            elif name:
                require(not _r_blank(name), f"Blank known array dim_name {array_id}")
            if metadata[4] == "0":
                same(unit, "", f"Absent array dim_units {array_id}")
            elif unit:
                require(not _r_blank(unit), f"Blank known array dim_unit {array_id}")
        if metadata[3] == "1":
            names = [row[3] for _index, row in axes if row[3] != ""]
            require(len(names) == len(set(names)), f"Duplicate known array axis names for {array_id}")

    array_point_groups = _check_point_groups(header_rows["ArrayPoints"], 0, 1, {key: index for index, key in enumerate(array_by_id)}, "ArrayPoints")
    for array_id, group in array_point_groups.items():
        array = array_by_id[array_id]
        same(len(group), _parse_uint(array[6], f"{array_id}.point_count", MAX_ROWS), f"Array point count {array_id}")
        axes = {index: row for index, row in axes_by_array[array_id]}
        for _index, row in group:
            _parse_typed_numeric(row[2], axes[1][2], f"{array_id}.location_1")
            _parse_typed_numeric(row[3], axes[2][2], f"{array_id}.location_2")
            _parse_typed_numeric(row[4], array[5], f"{array_id}.value")
            _parse_optional_positive(row[5], f"{array_id}.source_row")
            require(row[6] in {"", "0", "1"}, f"Invalid array saturation token for {array_id}")


def _parse_optional_positive(token: str, label: str) -> int | None:
    if token == "":
        return None
    value = _parse_int(token, label)
    require(value is not None and value > 0, f"{label} must be a positive integer or empty")
    return value


def _parse_typed_numeric(token: str, storage: str, label: str) -> int | float | None:
    if storage == "integer":
        if token == "NA":
            return None
        return _parse_int(token, label)
    require(storage == "double", f"Invalid numeric storage in {label}")
    if token == "NA":
        return None
    return _parse_double(token, label)


def read_bundle(bundle: Path, max_bytes: int = DEFAULT_MAX_BYTES) -> tuple[dict[str, str], dict[str, list[list[str]]]]:
    require(not bundle.is_symlink() and bundle.is_dir(), "Bundle must be a real directory")
    try:
        bundle.resolve(strict=True)
    except OSError as exc:
        raise QualificationError("Bundle directory does not exist") from exc
    remaining = max_bytes
    manifest_bytes = read_regular(bundle / "manifest.dcf", remaining)
    remaining -= len(manifest_bytes)
    manifest = _parse_manifest(manifest_bytes)
    delimiter = "," if manifest["Delimiter"] == "csv" else "\t"
    tables: dict[str, list[list[str]]] = {}
    for prefix, (stem, headers) in TABLES.items():
        require(remaining > 0, "Bundle exceeds the total actual-byte limit")
        name = f"{stem}.{manifest['Delimiter']}"
        payload = read_regular(bundle / name, remaining)
        remaining -= len(payload)
        same(sha256_bytes(payload), manifest[f"{prefix}-SHA256"], f"{prefix} payload hash")
        rows = _parse_csv(payload, delimiter, headers, name)
        same(len(rows) - 1, _parse_uint(manifest[f"{prefix}-Rows"], f"{prefix}-Rows", MAX_ROWS), f"{prefix} row count")
        tables[prefix] = rows
    _validate_bundle_tables(manifest, tables)
    return manifest, tables


def bundle_file_hashes(bundle: Path) -> dict[str, str]:
    require(not bundle.is_symlink() and bundle.is_dir(), "Bundle must be a real directory")
    result: dict[str, str] = {}
    try:
        manifest = read_regular(bundle / "manifest.dcf", 2 * 1024 * 1024)
        fields = _parse_manifest(manifest)
    except (QualificationError, OSError):
        fields = None
    if fields is not None:
        extension = fields["Delimiter"]
        names = ["manifest.dcf", *(f"{stem}.{extension}" for stem, _headers in TABLES.values())]
    else:
        names = ["manifest.dcf", *(f"{stem}.{extension}" for extension in ("csv", "tsv") for stem, _headers in TABLES.values())]
    for name in names:
        path = bundle / name
        if path.exists() or path.is_symlink():
            try:
                result[name] = sha256_file(path)
            except (QualificationError, OSError):
                result[name] = "unreadable"
    return result


def _canonical_table_order(rows: dict[str, list[list[str]]]) -> dict[str, list[list[str]]]:
    result = {prefix: [row.copy() for row in values] for prefix, values in rows.items()}
    index_columns = {prefix: TABLES[prefix][1].index("row_index") for prefix in ("Samples", "Injections", "Channels", "Sources", "Curves", "Streams", "Scans", "Arrays")}
    for prefix, index in index_columns.items():
        result[prefix].sort(key=lambda row: int(row[index]))
    curve_order = {row[1]: i for i, row in enumerate(result["Curves"])}
    scan_order = {row[1]: i for i, row in enumerate(result["Scans"])}
    array_order = {row[1]: i for i, row in enumerate(result["Arrays"])}
    result["Points"].sort(key=lambda row: (curve_order[row[0]], int(row[1])))
    result["ScanPoints"].sort(key=lambda row: (scan_order[row[0]], int(row[1])))
    result["ArrayAxes"].sort(key=lambda row: (array_order[row[0]], int(row[1])))
    result["ArrayPoints"].sort(key=lambda row: (array_order[row[0]], int(row[1])))
    return result


def _semantic_table_values(rows: dict[str, list[list[str]]]) -> dict[str, list[list[Any]]]:
    result: dict[str, list[list[Any]]] = {
        prefix: [row.copy() for row in values] for prefix, values in rows.items()
    }
    for row in result["Points"]:
        row[2] = _semantic_numeric_token(
            row[2], "double", "Points.location", normalize_positive_zero=True
        )
        row[3] = _semantic_numeric_token(
            row[3], "double", "Points.value", missing_empty=True,
            normalize_positive_zero=True,
        )
    for row in result["Scans"]:
        row[5] = _semantic_numeric_token(row[5], "double", "Scans.retention_time")

    scan_types = {row[1]: (row[10], row[11]) for row in result["Scans"]}
    for row in result["ScanPoints"]:
        location_type, value_type = scan_types[row[0]]
        row[2] = _semantic_numeric_token(row[2], location_type, "ScanPoints.location")
        row[3] = _semantic_numeric_token(row[3], value_type, "ScanPoints.value")

    array_types = {row[1]: row[5] for row in result["Arrays"]}
    axis_types: dict[str, dict[str, str]] = {}
    for row in result["ArrayAxes"]:
        axis_types.setdefault(row[0], {})[row[1]] = row[2]
    for row in result["ArrayPoints"]:
        axes = axis_types[row[0]]
        row[2] = _semantic_numeric_token(
            row[2], axes["1"], "ArrayPoints.location_1"
        )
        row[3] = _semantic_numeric_token(
            row[3], axes["2"], "ArrayPoints.location_2"
        )
        row[4] = _semantic_numeric_token(
            row[4], array_types[row[0]], "ArrayPoints.value"
        )
    return result


def canonical_tables(manifest: dict[str, str], tables: dict[str, list[list[str]]]) -> dict[str, list[list[str]]]:
    """Restore metadata and point order declared by independent keys."""
    rows = {prefix: [row.copy() for row in values[1:]] for prefix, values in tables.items()}
    return _canonical_table_order(rows)


def canonical_semantic_tables(
    tables: dict[str, list[list[str]]]
) -> dict[str, list[list[Any]]]:
    return _semantic_table_values(canonical_tables({}, tables))


def expected_semantic_tables(tables: dict[str, list[list[str]]]) -> dict[str, list[list[Any]]]:
    return _semantic_table_values(_canonical_table_order(tables))


def write_bundle(bundle: Path, object_id: str, delimiter_name: str, tables: dict[str, list[list[str]]]) -> None:
    require(delimiter_name in {"csv", "tsv"}, "Invalid output delimiter")
    require(not bundle.exists() and not bundle.is_symlink(), f"Refusing to overwrite control bundle {bundle}")
    bundle.mkdir(parents=True)
    delimiter = "," if delimiter_name == "csv" else "\t"
    hashes: dict[str, str] = {}
    for prefix, (stem, headers) in TABLES.items():
        rows = tables[prefix]
        require(all(len(row) == len(headers) for row in rows), f"Invalid expected rows in {prefix}")
        path = bundle / f"{stem}.{delimiter_name}"
        with path.open("x", encoding="utf-8", newline="") as stream:
            writer = csv.writer(stream, delimiter=delimiter, quoting=csv.QUOTE_ALL, lineterminator="\n")
            writer.writerow(headers)
            writer.writerows(rows)
            stream.flush()
            os.fsync(stream.fileno())
        hashes[prefix] = sha256_file(path)
    fields = {
        "Format": FORMAT,
        "Version": VERSION,
        "Delimiter": delimiter_name,
    }
    for prefix, (_stem, headers) in TABLES.items():
        fields[f"{prefix}-Rows"] = str(len(tables[prefix]))
        fields[f"{prefix}-Columns"] = str(len(headers))
        fields[f"{prefix}-SHA256"] = hashes[prefix]
    same(tuple(fields), MANIFEST_FIELDS, "Manifest writer order")
    manifest_path = bundle / "manifest.dcf"
    with manifest_path.open("x", encoding="utf-8", newline="") as stream:
        stream.write("".join(f"{key}: {value}\n" for key, value in fields.items()))
        stream.flush()
        os.fsync(stream.fileno())
    read_bundle(bundle)


def copy_bundle(source: Path, destination: Path) -> dict[str, str]:
    manifest, tables = read_bundle(source)
    del tables
    extension = manifest["Delimiter"]
    require(not destination.exists() and not destination.is_symlink(), f"Refusing to overwrite {destination}")
    destination.mkdir(parents=True)
    names = ["manifest.dcf", *(f"{stem}.{extension}" for stem, _headers in TABLES.values())]
    for name in names:
        payload = read_regular(source / name)
        with (destination / name).open("xb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
    return bundle_file_hashes(destination)


def _scan_double_values(vector: dict[str, Any], label: str, corpus_path: Path) -> tuple[str, list[tuple[str, Any]]]:
    storage, values = decode_vector(vector, label, corpus_path)
    if storage == "integer":
        return storage, [("na", None) if value is None else ("finite", value) for value in values]
    require(storage == "double", f"{label} must be numeric")
    normalized: list[tuple[str, Any]] = []
    for state, value in values:
        if state == "hex":
            normalized.append(("finite", value))
        else:
            normalized.append((state, value))
    return storage, normalized


def _trace_range(trace: dict[str, Any], corpus_path: Path) -> tuple[float, float] | None:
    raw = trace["mz_range"]
    if raw is None:
        return None
    storage, values = _scan_double_values(raw, "trace.mz_range", corpus_path)
    require(storage in {"integer", "double"} and len(values) == 2, "m/z range must be a length-two numeric vector")
    decoded = []
    for state, value in values:
        require(state == "finite" and value is not None and math.isfinite(value), "m/z range values must be finite")
        decoded.append(float(value))
    require(decoded[0] > 0 and decoded[0] <= decoded[1], "m/z range must satisfy 0 < lower <= upper")
    return decoded[0], decoded[1]


def calculate_trace(fixtures: dict[str, Any], trace: dict[str, Any], corpus_path: Path) -> tuple[list[dict[str, Any]], dict[str, Any]]:
    object_by_id = {item["id"]: item for item in fixtures["objects"]}
    require(trace["object_id"] in object_by_id, f"Unknown trace object {trace['object_id']}")
    obj = object_by_id[trace["object_id"]]
    selector_stream = trace["stream_id"]
    stream_rows = [row for row in obj["streams"] if row["stream_id"] == selector_stream]
    same(len(stream_rows), 1, f"Trace stream {selector_stream}")
    stream = stream_rows[0]
    same(stream["kind"], "ms", "Trace stream kind")
    polarity = trace["polarity"]
    level = trace["ms_level"]
    require(polarity in {"positive", "negative"}, "Trace requires explicit polarity")
    require(type(level) is int and level > 0, "Trace requires a positive integer MS level")
    mz_range = _trace_range(trace, corpus_path)
    operation = "tic" if mz_range is None else "xic"
    if mz_range is not None:
        require(stream["mz_unit"] is not None, "XIC requires a known stream m/z unit")
    require(stream["time_unit"] is not None, "Trace requires a known stream time unit")

    matching: list[dict[str, Any]] = []
    for scan_row in obj["scans"]:
        if scan_row["stream_id"] != selector_stream:
            continue
        polarity_value = scan_row["polarity"]
        level_value = scan_row["ms_level"]
        polarity_possible = polarity_value is None or polarity_value == polarity
        level_possible = level_value is None or level_value == level
        if polarity_possible and level_possible:
            if polarity_value is None or level_value is None:
                raise QualificationError(f"Ambiguous selector metadata in scan {scan_row['scan_id']}")
        if polarity_value == polarity and level_value == level:
            matching.append(scan_row)
    require(bool(matching), f"No scans match trace selectors for {selector_stream}")
    matching.sort(key=lambda row: row["scan_index"])

    result: list[dict[str, Any]] = []
    for scan_row in matching:
        rt_state, rt_value = _decode_atom(scan_row["retention_time"], f"{scan_row['scan_id']}.retention_time")
        require(rt_state == "hex" and rt_value is not None and math.isfinite(rt_value), f"Selected scan {scan_row['scan_id']} has unknown or nonfinite retention time")
        contributor_indices: list[int] = []
        contributor_values: list[tuple[str, Any]] = []
        masks: list[bool | None] = []
        mz_values: list[tuple[str, Any]] = []
        if scan_row["scan_status"] == "acquired":
            require(scan_row["spectrum_type"] == "centroid", f"Selected acquired scan {scan_row['scan_id']} is not centroid")
            location_storage, mz_values = _scan_double_values(scan_row["measures"]["location"], f"{scan_row['scan_id']}.location", corpus_path)
            value_storage, intensity_values = _scan_double_values(scan_row["measures"]["value"], f"{scan_row['scan_id']}.value", corpus_path)
            require(location_storage in {"integer", "double"} and value_storage in {"integer", "double"}, "Scan vectors must be numeric")
            require(len(mz_values) == len(intensity_values), f"Raw scan vectors differ in length for {scan_row['scan_id']}")
            for point_index, (state, mz) in enumerate(mz_values, 1):
                require(state == "finite" and mz is not None and math.isfinite(float(mz)) and float(mz) > 0.0, f"Invalid m/z in selected acquired scan {scan_row['scan_id']}")
            if scan_row["scan_status"] == "acquired" and not mz_values:
                pass
            if mz_range is None:
                chosen = list(range(len(mz_values)))
            else:
                lower, upper = mz_range
                chosen = [index for index, (_state, mz) in enumerate(mz_values) if lower <= float(mz) <= upper]
            source_rows = scan_row["source_rows"]
            saturated = scan_row["saturated"]
            require(len(source_rows) == len(mz_values) and len(saturated) == len(mz_values), f"Scan annotations differ in length for {scan_row['scan_id']}")
            for index in chosen:
                state, value = intensity_values[index]
                if state != "na":
                    require(state == "finite" and value is not None and math.isfinite(float(value)) and float(value) >= 0.0, f"Invalid selected intensity in scan {scan_row['scan_id']}")
                contributor_indices.append(index + 1)
                contributor_values.append((state, value))
                masks.append(saturated[index])
        elif scan_row["scan_status"] == "missing":
            require(len(scan_row["source_rows"]) == len(scan_row["saturated"]) == 0, f"Missing scan {scan_row['scan_id']} has point annotations")
        else:
            raise QualificationError(f"Invalid scan status in {scan_row['scan_id']}")

        saturation_unknown = any(value is None for value in masks)
        total = 0.0
        for state, value in contributor_values:
            if state == "na":
                continue
            total += float(value)
            require(math.isfinite(total), f"Centroid sum is not representable in {scan_row['scan_id']}")

        if scan_row["scan_status"] == "missing":
            value_atom = {"state": "na"}
            status = "missing_scan"
        elif len(mz_values) == 0:
            value_atom = _atom_from_float(0.0)
            status = "empty_scan"
        elif not contributor_indices:
            value_atom = _atom_from_float(0.0)
            status = "empty_window"
        elif any(mask is True for mask in masks):
            value_atom = {"state": "na"}
            status = "saturated"
        elif any(state == "na" for state, _value in contributor_values):
            value_atom = {"state": "na"}
            status = "missing_intensity"
        else:
            value_atom = _atom_from_float(total)
            status = "calculated"
        result.append({
            "scan_id": scan_row["scan_id"],
            "scan_index": scan_row["scan_index"],
            "source_scan_id": scan_row["source_scan_id"],
            "retention_time": scan_row["retention_time"],
            "value": value_atom,
            "status": status,
            "selected_points": len(contributor_indices),
            "saturation_unknown": saturation_unknown,
            "point_indices": contributor_indices,
        })

    record = fixtures["record_templates"][obj["record_template"]]
    source_sha = None
    if stream["source_id"] is not None:
        source_rows = [row for row in record["sources"] if row["source_id"] == stream["source_id"]]
        require(len(source_rows) == 1, f"Unknown trace source {stream['source_id']}")
        source_sha = source_rows[0]["source_sha256"]
    processing = {
        "version": 1,
        "operation": operation,
        "package": "measure",
        "package_version": None,
        "stream_id": stream["stream_id"],
        "injection_id": stream["injection_id"],
        "channel_id": stream["channel_id"],
        "source_id": stream["source_id"],
        "source_sha256": source_sha,
        "polarity": polarity,
        "ms_level": level,
        "mz_range": None if mz_range is None else {
            "storage": "double",
            "data": [_atom_from_float(mz_range[0]), _atom_from_float(mz_range[1])],
        },
        "mz_unit": stream["mz_unit"],
        "time_unit": stream["time_unit"],
        "value_name": stream["value_name"],
        "value_unit": stream["value_unit"],
        "aggregation": "centroid_sum",
        "missing_intensity": "propagate",
        "saturation": "propagate",
        "synchronization": "none",
    }
    return result, processing


def validate_declared_traces(fixtures: dict[str, Any], corpus_path: Path) -> dict[str, tuple[list[dict[str, Any]], dict[str, Any]]]:
    result: dict[str, tuple[list[dict[str, Any]], dict[str, Any]]] = {}
    for trace in fixtures["traces"]:
        rows, processing = calculate_trace(fixtures, trace, corpus_path)
        same(rows, trace["expected"], f"Hand-declared trace {trace['id']}")
        result[trace["id"]] = rows, processing
    for trace in fixtures["semantic_rejections"]:
        try:
            calculate_trace(fixtures, trace, corpus_path)
        except QualificationError:
            same(trace["expected"], "error", f"Semantic rejection {trace['id']}")
        else:
            raise QualificationError(f"Declared semantic rejection unexpectedly succeeded: {trace['id']}")
    return result


def runtime_check(value: Any) -> None:
    keys(value, ("profile", "R", "installed_package", "tool_checkout_head", "created_utc", "session_info", "dependency_versions"), "runtime")
    same(value["profile"], PROFILE, "Runtime profile")
    require(type(value["R"]) is str and value["R"].startswith("R version "), "Invalid R version string")
    package = value["installed_package"]
    keys(package, ("path", "namespace_path", "version", "built", "remote_sha"), "installed package")
    path = package["path"]
    require(
        type(path) is str
        and "\x00" not in path
        and (PurePosixPath(path).is_absolute() or PureWindowsPath(path).is_absolute()),
        "Installed package path is not an absolute POSIX or Windows path",
    )
    same(package["namespace_path"], path, "Installed namespace path")
    require(type(package["version"]) is str and package["version"], "Missing installed package version")
    for field in ("built", "remote_sha"):
        require(package[field] is None or type(package[field]) is str, f"Invalid installed package {field}")
    head = value["tool_checkout_head"]
    require(head is None or type(head) is str and len(head) in {40, 64} and re.fullmatch(r"[0-9a-f]+", head) is not None, "Invalid tool checkout HEAD")
    session = value["session_info"]
    require(type(session) is list and session and all(type(item) is str for item in session), "Invalid R session information")
    same(session[0], value["R"], "Runtime session R version")
    deps = value["dependency_versions"]
    require(type(deps) is dict and set(deps) >= {"measure", "vctrs", "jsonlite", "digest", "fs"}, "Missing runtime dependencies")
    require(all(type(version) is str and version for version in deps.values()), "Invalid dependency version")
    same(deps["measure"], package["version"], "Installed measure dependency version")
    require(type(value["created_utc"]) is str and value["created_utc"], "Missing runtime timestamp")


def status_check(root: Path, filename: str, phase: str) -> None:
    value = read_json(root / filename, 128 * 1024)
    keys(value, ("phase", "complete", "stage", "error", "updated_utc"), filename)
    same(value["phase"], phase, f"{phase} phase")
    require(value["complete"] is True and value["stage"] == "complete" and value["error"] is None, f"{phase} phase is incomplete")
    require(type(value["updated_utc"]) is str and value["updated_utc"], f"Missing {phase} timestamp")


def progress_check(root: Path, filename: str, phase: str, expected_completed: list[str]) -> None:
    value = read_json(root / filename, 128 * 1024)
    keys(value, ("phase", "complete", "stage", "current_case", "completed_bundles", "error", "updated_utc"), filename)
    same(value["phase"], phase, f"{phase} progress phase")
    require(value["complete"] is True and value["stage"] == "complete" and value["current_case"] is None and value["error"] is None, f"{phase} progress is incomplete")
    completed = value["completed_bundles"]
    require(
        type(completed) is list and all(type(item) is str for item in completed)
        and len(completed) == len(set(completed)),
        f"Invalid {phase} completed item list",
    )
    same(completed, expected_completed, f"{phase} completed item coverage")
    require(type(value["updated_utc"]) is str and value["updated_utc"], f"Invalid {phase} progress")


def producer_completed_ids(fixtures: dict[str, Any], declaration: dict[str, Any]) -> list[str]:
    return [
        *(row["id"] for row in declaration["forward"]),
        *(row["id"] for row in fixtures["traces"]),
        *(row["id"] for row in fixtures["semantic_rejections"]),
        "legacy-controls",
    ]


def mutation_completed_ids(declaration: dict[str, Any]) -> list[str]:
    return [
        *(row["id"] for row in declaration["controls"]),
        *(row["id"] for row in declaration["invalid"]),
        "mutations.json",
    ]


def rejection_completed_ids(declaration: dict[str, Any]) -> list[str]:
    return [
        *(row["id"] for row in declaration["controls"]),
        *(row["id"] for row in declaration["invalid"]),
        *(row["id"] for row in declaration["cross_format"]),
    ]


def _hashes(root: Path, names: tuple[str, ...]) -> dict[str, str]:
    return {name: sha256_file(root / name) for name in names}


def _expected_processing(base: dict[str, Any], package_version: str) -> dict[str, Any]:
    result = dict(base)
    result["package_version"] = package_version
    return result


def frozen_input_check(output_root: Path, tool_dir: Path, input_hashes: dict[str, str]) -> None:
    frozen = output_root / "frozen-inputs"
    require(not frozen.is_symlink() and frozen.is_dir(), "Output frozen-inputs must be a real directory")
    frozen_names = {entry.name for entry in frozen.iterdir()}
    same(frozen_names, {*FIXED_INPUT_NAMES, "input-manifest.json"}, "Copied frozen-input names")
    for name in (*FIXED_INPUT_NAMES, "input-manifest.json"):
        source_bytes = read_regular(tool_dir / name)
        copied_bytes = read_regular(frozen / name, len(source_bytes))
        same(len(copied_bytes), len(source_bytes), f"Copied input size {name}")
        expected_hash = sha256_bytes(source_bytes)
        same(sha256_bytes(copied_bytes), expected_hash, f"Copied input hash {name}")
        same(copied_bytes, source_bytes, f"Copied input bytes {name}")
        if name in input_hashes:
            same(expected_hash, input_hashes[name], f"Frozen input binding {name}")


def verify_producer(root: Path, fixtures: dict[str, Any], declaration: dict[str, Any], input_hashes: dict[str, str], corpus_path: Path) -> dict[str, Any]:
    status_check(root, "run-status.json", "produce")
    progress_check(root, "progress.json", "produce", producer_completed_ids(fixtures, declaration))
    marker = read_json(root / "qualification-root.json", 128 * 1024)
    keys(marker, ("format", "declaration_sha256", "input_manifest_sha256"), "qualification root marker")
    same(marker["format"], PROFILE, "Root marker format")
    same(marker["declaration_sha256"], input_hashes["declaration.json"], "Root declaration binding")
    same(marker["input_manifest_sha256"], sha256_file(Path(__file__).resolve().parent / "input-manifest.json"), "Root input manifest binding")
    frozen_input_check(root, Path(__file__).resolve().parent, input_hashes)

    runtime = read_json(root / "runtime.json", 2 * 1024 * 1024)
    runtime_check(runtime)
    observed_document = read_json(root / "observations.json", DEFAULT_MAX_BYTES)
    keys(observed_document, ("profile", "objects"), "observations")
    same(observed_document["profile"], PROFILE, "Observations profile")
    expected_objects = expected_all_objects(fixtures, declaration, corpus_path)
    observed_objects = observed_document["objects"]
    require(type(observed_objects) is list and len(observed_objects) == len(expected_objects), "Wrong observation object count")
    for object_id, actual in zip(expected_objects, observed_objects, strict=True):
        same(actual, expected_objects[object_id], f"R object observation {object_id}")

    for descriptor in declaration["forward"]:
        manifest, tables = read_bundle(root / descriptor["path"])
        same(manifest["Delimiter"], descriptor["delimiter"], f"Bundle delimiter {descriptor['id']}")
        actual = canonical_semantic_tables(tables)
        expected = _expected_tables_for_object(fixtures, descriptor["object_id"], corpus_path)
        same(actual, expected_semantic_tables(expected), f"Forward bundle {descriptor['id']}")

    trace_doc = read_json(root / "trace-results.json", 16 * 1024 * 1024)
    keys(trace_doc, ("profile", "cases"), "trace results")
    same(trace_doc["profile"], PROFILE, "Trace result profile")
    trace_expectations = validate_declared_traces(fixtures, corpus_path)
    require(type(trace_doc["cases"]) is list and len(trace_doc["cases"]) == len(fixtures["traces"]), "Wrong trace result case count")
    for actual, trace in zip(trace_doc["cases"], fixtures["traces"], strict=True):
        keys(actual, ("id", "object_id", "rows", "processing"), "trace result case")
        same(actual["id"], trace["id"], "Trace result id")
        same(actual["object_id"], trace["object_id"], "Trace result object id")
        rows, processing = trace_expectations[trace["id"]]
        same(actual["rows"], rows, f"Trace result rows {trace['id']}")
        same(actual["processing"], _expected_processing(processing, runtime["installed_package"]["version"]), f"Trace processing {trace['id']}")

    rejection_doc = read_json(root / "semantic-rejections.json", 2 * 1024 * 1024)
    keys(rejection_doc, ("profile", "cases"), "semantic rejection results")
    same(rejection_doc["profile"], PROFILE, "Semantic rejection profile")
    require(type(rejection_doc["cases"]) is list and len(rejection_doc["cases"]) == len(fixtures["semantic_rejections"]), "Wrong semantic rejection count")
    for actual, declared in zip(rejection_doc["cases"], fixtures["semantic_rejections"], strict=True):
        keys(actual, ("id", "error", "message"), "semantic rejection")
        same(actual["id"], declared["id"], "Semantic rejection id")
        require(actual["error"] is True and type(actual["message"]) is str and actual["message"].strip(), f"Missing actual error for {declared['id']}")
    return runtime
