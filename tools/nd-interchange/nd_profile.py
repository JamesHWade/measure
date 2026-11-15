"""Strict standard-library support for measure nD interchange qualification."""

import hashlib
import json
import math
import os
import re
import shutil
import stat
import struct
import tempfile
from pathlib import Path


PROFILE = "measure-nd-interchange/1"
FORMAT = "measure-nd"
VERSION = "1"
DEFAULT_MAX_BYTES = 100 * 1024 * 1024
MAX_DIMENSIONS = 1024
MAX_INTEGER = 2_147_483_647
MIN_INTEGER = -2_147_483_647
MAX_EXPECTED_VALUES = 1_000_000

FROZEN_INPUTS = {
    "declaration.json": {
        "sha256": "2963f9966b77721682a48c9dd60b009b43195577e56f461ebaa51255f452bc9c",
        "size_bytes": 6138,
    },
    "finite-stress.bin": {
        "sha256": "d11301fb08fce7aab29a12ab49e82b32c8438c7bdd0b9489a5ff49587e3cf63c",
        "size_bytes": 65664,
    },
    "fixtures.json": {
        "sha256": "8ec764cc0f77e8540a5764f37ad5a9784132f31910101005b508f14dd993f57b",
        "size_bytes": 53739,
    },
}

MANIFEST_FIELDS = (
    "Format",
    "Version",
    "Delimiter",
    "Object-Type",
    "Dimensions",
    "List-Names",
    "Members-Rows",
    "Members-Columns",
    "Members-SHA256",
    "Axes-Rows",
    "Axes-Columns",
    "Axes-SHA256",
    "Points-Rows",
    "Points-Columns",
    "Points-SHA256",
)
MEMBER_COLUMNS = (
    "member_index",
    "member_name",
    "member_name_missing",
    "dim_names_present",
    "dim_units_present",
    "value_type",
    "point_count",
)
AXIS_COLUMNS = (
    "member_index",
    "axis_index",
    "storage_type",
    "axis_name",
    "axis_unit",
)

_UNSIGNED = re.compile(r"(?:0|[1-9][0-9]*)\Z")
_INTEGER = re.compile(r"(?:0|-[1-9][0-9]*|[1-9][0-9]*)\Z")
_DOUBLE = re.compile(r"[+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?\Z")
_HEX64 = re.compile(r"[0-9a-f]{16}\Z")
_SHA256 = re.compile(r"[0-9a-f]{64}\Z")
_FIELD_NAME = re.compile(r"[A-Za-z][A-Za-z0-9-]*\Z")
_R_HV_WHITESPACE = frozenset(
    "\t \u00a0\u1680\u180e"
    "\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a"
    "\u202f\u205f\u3000\n\v\f\r\u0085\u2028\u2029"
)


def _fail(message):
    raise ValueError(message)


def _check_limit(max_bytes):
    if type(max_bytes) is not int or max_bytes <= 0 or max_bytes >= 2**31:
        _fail("max_bytes must be a positive bounded integer")
    return max_bytes


def _open_regular(path):
    """Open a nonsymlink regular file without blocking on a raced FIFO."""
    path = Path(path)
    try:
        before = path.lstat()
    except OSError as exc:
        raise ValueError(f"file is missing or unreadable: {path}") from exc
    if stat.S_ISLNK(before.st_mode) or not stat.S_ISREG(before.st_mode):
        _fail(f"file must be a nonsymlink regular file: {path}")

    flags = os.O_RDONLY
    flags |= getattr(os, "O_NOFOLLOW", 0)
    flags |= getattr(os, "O_NONBLOCK", 0)
    try:
        descriptor = os.open(path, flags)
    except OSError as exc:
        raise ValueError(f"file is not safely readable: {path}") from exc
    try:
        after = os.fstat(descriptor)
        if not stat.S_ISREG(after.st_mode):
            _fail(f"file must be a nonsymlink regular file: {path}")
        return os.fdopen(descriptor, "rb"), after.st_size
    except Exception:
        os.close(descriptor)
        raise


def _read_regular_bytes(path, max_bytes=DEFAULT_MAX_BYTES):
    max_bytes = _check_limit(max_bytes)
    stream, _size = _open_regular(path)
    with stream:
        data = stream.read(max_bytes + 1)
    if len(data) > max_bytes:
        _fail(f"file exceeds max_bytes while reading: {path}")
    return data


def read_bytes(path, max_bytes=DEFAULT_MAX_BYTES):
    """Read exact bytes from a bounded nonsymlink regular file."""
    return _read_regular_bytes(path, max_bytes)


def _json_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            _fail(f"JSON contains a duplicate object key: {key}")
        result[key] = value
    return result


def _json_float(token):
    value = float(token)
    if not math.isfinite(value):
        _fail("JSON contains a floating-point value outside the finite range")
    significand = re.split(r"[eE]", token, maxsplit=1)[0]
    if value == 0.0 and re.search(r"[1-9]", significand):
        _fail("JSON contains a nonzero floating-point value that underflows to zero")
    return value


def _json_constant(token):
    _fail(f"JSON contains a nonstandard numeric constant: {token}")


def _loads_json(data, label):
    if b"\x00" in data:
        _fail(f"JSON input contains a NUL byte: {label}")
    try:
        text = data.decode("utf-8", errors="strict")
    except UnicodeDecodeError as exc:
        raise ValueError(f"JSON input is not valid UTF-8: {label}") from exc
    try:
        value = json.loads(
            text,
            object_pairs_hook=_json_pairs,
            parse_float=_json_float,
            parse_constant=_json_constant,
        )
    except (json.JSONDecodeError, UnicodeError) as exc:
        raise ValueError(f"invalid JSON: {label}: {exc}") from exc
    if _contains_nul(value):
        _fail(f"JSON input contains a NUL character: {label}")
    return value


def _contains_nul(value):
    if type(value) is str:
        return "\x00" in value
    if type(value) is list:
        return any(_contains_nul(item) for item in value)
    if type(value) is dict:
        return any(
            _contains_nul(key) or _contains_nul(item) for key, item in value.items()
        )
    return False


def read_json(path, max_bytes=DEFAULT_MAX_BYTES):
    """Read a bounded regular UTF-8 JSON file with duplicate/nonfinite checks."""
    return _loads_json(_read_regular_bytes(path, max_bytes), path)


def sha256_file(path):
    """Hash exact bytes from a bounded nonsymlink regular file."""
    return hashlib.sha256(_read_regular_bytes(path)).hexdigest()


def write_json_exclusive(path, obj):
    """Create a UTF-8 JSON receipt without replacing any existing path."""
    path = Path(path)
    try:
        payload = (
            json.dumps(
                obj,
                ensure_ascii=False,
                allow_nan=False,
                indent=2,
                separators=(",", ": "),
            )
            + "\n"
        ).encode("utf-8", errors="strict")
    except (TypeError, ValueError, UnicodeError) as exc:
        raise ValueError(f"receipt is not strict UTF-8 JSON: {exc}") from exc

    flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(path, flags, 0o666)
    except FileExistsError:
        _fail(f"refusing to overwrite an existing JSON receipt: {path}")
    except OSError as exc:
        raise ValueError(f"could not create JSON receipt exclusively: {path}") from exc
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
    except Exception:
        try:
            path.unlink()
        except OSError:
            pass
        raise


def _require_keys(value, required, optional=(), label="object"):
    if type(value) is not dict:
        _fail(f"{label} must be a JSON object")
    keys = set(value)
    expected = set(required)
    allowed = expected | set(optional)
    if not expected <= keys or not keys <= allowed:
        _fail(f"{label} has missing or unknown fields")


def _require_str(value, label, allow_empty=True):
    if type(value) is not str or (not allow_empty and not value):
        _fail(f"{label} must be a plain string")
    try:
        value.encode("utf-8", errors="strict")
    except UnicodeError as exc:
        raise ValueError(f"{label} is not valid Unicode") from exc
    if "\x00" in value:
        _fail(f"{label} must not contain NUL")
    return value


def _require_int(value, label, minimum=None, maximum=None):
    if type(value) is not int:
        _fail(f"{label} must be an integer")
    if minimum is not None and value < minimum:
        _fail(f"{label} is below its supported minimum")
    if maximum is not None and value > maximum:
        _fail(f"{label} exceeds its supported maximum")
    return value


def _require_array(value, label):
    if type(value) is not list:
        _fail(f"{label} must be an array")
    return value


def load_inputs(script_dir):
    """Validate the frozen input manifest and its three hardcoded artifacts."""
    script_dir = Path(script_dir).resolve(strict=True)
    manifest_path = script_dir / "input-manifest.json"
    manifest = read_json(manifest_path, max_bytes=32 * 1024)
    _require_keys(manifest, ("profile", "files"), label="input manifest")
    if manifest["profile"] != PROFILE:
        _fail("frozen input manifest has the wrong profile")
    files = manifest["files"]
    _require_keys(files, tuple(FROZEN_INPUTS), label="input file manifest")

    hashes = {}
    contents = {}
    for name, expected in FROZEN_INPUTS.items():
        item = files[name]
        _require_keys(item, ("sha256", "size_bytes"), label=f"input manifest {name}")
        if (
            item["sha256"] != expected["sha256"]
            or item["size_bytes"] != expected["size_bytes"]
        ):
            _fail(f"input manifest does not match the frozen declaration for {name}")
        data = _read_regular_bytes(script_dir / name, expected["size_bytes"])
        if len(data) != expected["size_bytes"]:
            _fail(f"frozen input has the wrong byte size: {name}")
        digest = hashlib.sha256(data).hexdigest()
        if digest != expected["sha256"]:
            _fail(f"frozen input hash does not match: {name}")
        hashes[name] = digest
        contents[name] = data

    fixtures = _loads_json(contents["fixtures.json"], "fixtures.json")
    declaration = _loads_json(contents["declaration.json"], "declaration.json")
    _validate_fixture_index(fixtures, declaration)
    return fixtures, declaration, hashes


def _validate_fixture_index(fixtures, declaration):
    _require_keys(fixtures, ("profile", "objects", "finite_corpus"), label="fixtures")
    _require_keys(
        declaration,
        (
            "profile",
            "formats",
            "object_ids",
            "forward_bundles",
            "reverse_bundles",
            "reordered_bundles",
            "invalid_bundles",
            "cross_format_rejections",
            "source_values",
            "fidelity",
            "scope",
        ),
        label="declaration",
    )
    if fixtures["profile"] != PROFILE or declaration["profile"] != PROFILE:
        _fail("frozen fixture profile mismatch")
    objects = _require_array(fixtures["objects"], "fixtures.objects")
    ids = []
    for item in objects:
        _require_keys(
            item,
            ("id", "kind", "ndim", "list_names", "ptype", "members"),
            optional=("construction",),
            label="fixture object",
        )
        ident = _require_str(item["id"], "fixture id", allow_empty=False)
        if ident in ids:
            _fail(f"duplicate fixture id: {ident}")
        ids.append(ident)
    if ids != declaration["object_ids"]:
        _fail("fixture object order or ids differ from the frozen declaration")
    if declaration["formats"] != ["csv", "tsv"]:
        _fail("unexpected declared delimiter set")
    _validate_declaration_cases(declaration, set(ids))


def _validate_declaration_cases(declaration, object_ids):
    seen_forward = set()
    for case in _require_array(declaration["forward_bundles"], "forward_bundles"):
        _require_keys(case, ("id", "object_id", "delimiter"), label="forward bundle")
        ident = _require_str(case["id"], "bundle id", allow_empty=False)
        object_id = case["object_id"]
        if ident in seen_forward or object_id not in object_ids:
            _fail("invalid or duplicate forward bundle reference")
        if case["delimiter"] not in ("csv", "tsv"):
            _fail("unsupported declared delimiter")
        seen_forward.add(ident)
    expected_forward = {
        f"{object_id}-{delimiter}"
        for object_id in object_ids
        for delimiter in ("csv", "tsv")
    }
    if seen_forward != expected_forward:
        _fail("forward bundle cases do not cover every frozen object and delimiter")
    seen_control = set()
    for cases_key in ("reverse_bundles", "reordered_bundles"):
        for case in _require_array(declaration[cases_key], cases_key):
            if cases_key == "reverse_bundles":
                _require_keys(case, ("id", "object_id", "delimiter"), label=cases_key)
            else:
                _require_keys(case, ("id", "base_bundle", "object_id"), label=cases_key)
            ident = _require_str(case["id"], f"{cases_key} id", allow_empty=False)
            if ident in seen_control or case["object_id"] not in object_ids:
                _fail(f"invalid or duplicate {cases_key} reference")
            if cases_key == "reverse_bundles" and case["delimiter"] not in (
                "csv",
                "tsv",
            ):
                _fail("unsupported reverse bundle delimiter")
            seen_control.add(ident)
    invalid = _require_array(declaration["invalid_bundles"], "invalid_bundles")
    if any(type(ident) is not str or not ident for ident in invalid) or len(
        set(invalid)
    ) != len(invalid):
        _fail("invalid bundle ids must be unique nonempty strings")
    cross = _require_array(
        declaration["cross_format_rejections"], "cross_format_rejections"
    )
    if any(type(ident) is not str or not ident for ident in cross) or len(
        set(cross)
    ) != len(cross):
        _fail("cross-format ids must be unique nonempty strings")


def _validate_metadata(metadata, ndim, label, unique=False):
    if metadata is None:
        return None
    _require_array(metadata, label)
    if len(metadata) != ndim:
        _fail(f"{label} must have one entry per dimension")
    result = []
    seen = set()
    for value in metadata:
        if value is not None:
            _require_str(value, label)
            if _r_metadata_blank(value):
                _fail(f"known {label} entries must not be blank")
            if unique and value in seen:
                _fail(f"known {label} entries must be unique")
            seen.add(value)
        result.append(value)
    return result


def _r_metadata_blank(value):
    return not value or all(character in _R_HV_WHITESPACE for character in value)


def _decode_hex64(token, label):
    if type(token) is not str or not _HEX64.fullmatch(token):
        _fail(f"{label} must be 16 lowercase hexadecimal digits")
    raw = bytes.fromhex(token)
    value = struct.unpack(">d", raw)[0]
    if not math.isfinite(value):
        _fail(f"{label} encodes a nonfinite double instead of a state token")
    return value


def _validate_fixture_token(token, storage, label):
    if type(token) is not dict or type(token.get("state")) is not str:
        _fail(f"{label} must be a state token object")
    state = token["state"]
    if state == "number":
        if storage == "integer":
            _require_keys(token, ("state", "number"), label=label)
            number = _require_int(
                token["number"], f"{label} integer", MIN_INTEGER, MAX_INTEGER
            )
            return {"state": "number", "number": number}
        if storage == "double":
            _require_keys(token, ("state", "hex"), label=label)
            number_hex = token["hex"]
            _decode_hex64(number_hex, label)
            return {"state": "number", "hex": number_hex}
        _fail(f"{label} uses an unsupported storage type")

    _require_keys(token, ("state",), label=label)
    if storage == "integer":
        allowed = {"na"}
    elif storage == "double":
        allowed = {"na", "nan", "+inf", "-inf"}
    else:
        _fail(f"{label} uses an unsupported storage type")
    if state not in allowed:
        _fail(f"{label} has an invalid state for {storage}")
    return {"state": state}


def _token_from_binary64(raw, label):
    if len(raw) != 8:
        _fail(f"{label} is not a binary64 word")
    token = raw.hex()
    _decode_hex64(token, label)
    return {"state": "number", "hex": token}


def _expand_column(column, ndim, script_dir, binary_values, label):
    encoding = column.get("encoding")
    storage = column.get("storage")
    name = column.get("name")
    if type(name) is not str or name not in (
        *[f"location_{i}" for i in range(1, ndim + 1)],
        "value",
    ):
        _fail(f"{label} has a noncanonical column name")
    if storage not in ("integer", "double"):
        _fail(f"{label}.storage must be integer or double")

    if encoding == "values":
        _require_keys(column, ("name", "storage", "encoding", "data"), label=label)
        tokens = [
            _validate_fixture_token(token, storage, f"{label}.data[{i}]")
            for i, token in enumerate(_require_array(column["data"], f"{label}.data"))
        ]
    elif encoding == "sequence":
        _require_keys(
            column, ("name", "storage", "encoding", "start", "length"), label=label
        )
        if storage != "integer":
            _fail(f"{label} sequence encoding requires integer storage")
        start = _require_int(
            column["start"], f"{label}.start", MIN_INTEGER, MAX_INTEGER
        )
        length = _require_int(
            column["length"], f"{label}.length", 0, MAX_EXPECTED_VALUES
        )
        end = start + length - 1
        if length and not MIN_INTEGER <= end <= MAX_INTEGER:
            _fail(f"{label} sequence exceeds the integer storage range")
        tokens = [
            {"state": "number", "number": start + offset} for offset in range(length)
        ]
    elif encoding == "cycle":
        _require_keys(
            column, ("name", "storage", "encoding", "data", "length"), label=label
        )
        if storage != "double":
            _fail(f"{label} cycle encoding requires double storage")
        pattern = [
            _validate_fixture_token(token, storage, f"{label}.data[{i}]")
            for i, token in enumerate(_require_array(column["data"], f"{label}.data"))
        ]
        length = _require_int(
            column["length"], f"{label}.length", 0, MAX_EXPECTED_VALUES
        )
        if length and not pattern:
            _fail(f"{label} cannot cycle an empty pattern")
        tokens = (
            [pattern[i % len(pattern)].copy() for i in range(length)] if length else []
        )
    elif encoding == "binary64":
        _require_keys(
            column,
            ("name", "storage", "encoding", "file", "endian", "length"),
            label=label,
        )
        if (
            storage != "double"
            or column["file"] != "finite-stress.bin"
            or column["endian"] != "big"
        ):
            _fail(f"{label} has an unsupported binary64 source")
        length = _require_int(
            column["length"], f"{label}.length", 0, MAX_EXPECTED_VALUES
        )
        if binary_values is None:
            _fail("finite-stress.bin was not loaded")
        if len(binary_values) != length * 8:
            _fail(f"{label} binary64 length does not match the frozen corpus")
        tokens = [
            _token_from_binary64(
                binary_values[offset : offset + 8], f"{label}[{offset // 8}]"
            )
            for offset in range(0, len(binary_values), 8)
        ]
    else:
        _fail(f"{label} has an unknown encoding")

    return {"name": name, "storage": storage, "data": tokens}


def _canonical_fixture_table(table, ndim, script_dir, binary_values, label):
    _require_keys(table, ("columns", "dim_names", "dim_units"), label=label)
    dim_names = _validate_metadata(
        table["dim_names"], ndim, f"{label}.dim_names", unique=True
    )
    dim_units = _validate_metadata(table["dim_units"], ndim, f"{label}.dim_units")
    columns = _require_array(table["columns"], f"{label}.columns")
    expected_names = [f"location_{axis}" for axis in range(1, ndim + 1)] + ["value"]
    if len(columns) != ndim + 1:
        _fail(f"{label} must have D location columns and one value column")
    expanded = [
        _expand_column(column, ndim, script_dir, binary_values, f"{label}.columns[{i}]")
        for i, column in enumerate(columns)
    ]
    if [column["name"] for column in expanded] != expected_names:
        _fail(f"{label} columns are not in canonical order")
    lengths = {len(column["data"]) for column in expanded}
    if len(lengths) != 1:
        _fail(f"{label} column lengths differ")
    return {
        "columns": expanded,
        "dim_names": dim_names,
        "dim_units": dim_units,
    }


def expected_objects(fixtures, script_dir):
    """Expand frozen tokens and committed big-endian words into truth objects."""
    script_dir = Path(script_dir).resolve(strict=True)
    _validate_fixture_index(
        fixtures,
        {
            "profile": PROFILE,
            "object_ids": [item.get("id") for item in fixtures.get("objects", [])]
            if type(fixtures) is dict
            else [],
            "formats": ["csv", "tsv"],
            "forward_bundles": [
                {
                    "id": f"{item.get('id')}-{delimiter}",
                    "object_id": item.get("id"),
                    "delimiter": delimiter,
                }
                for item in fixtures.get("objects", [])
                for delimiter in ("csv", "tsv")
            ]
            if type(fixtures) is dict
            else [],
            "reverse_bundles": [],
            "reordered_bundles": [],
            "invalid_bundles": [],
            "cross_format_rejections": [],
            "source_values": "",
            "fidelity": "",
            "scope": "",
        },
    )
    corpus = fixtures["finite_corpus"]
    _require_keys(
        corpus,
        (
            "file",
            "count",
            "fixed_controls",
            "random_finite_values",
            "generator",
            "seed",
            "generator_python",
            "scope",
        ),
        label="finite corpus",
    )
    if (
        corpus["file"] != "finite-stress.bin"
        or corpus["count"] != 8208
        or corpus["fixed_controls"] != 16
        or corpus["random_finite_values"] != 8192
    ):
        _fail("unexpected finite stress corpus declaration")
    corpus_bytes = _read_regular_bytes(
        script_dir / "finite-stress.bin",
        FROZEN_INPUTS["finite-stress.bin"]["size_bytes"],
    )
    if (
        hashlib.sha256(corpus_bytes).hexdigest()
        != FROZEN_INPUTS["finite-stress.bin"]["sha256"]
    ):
        _fail("finite-stress.bin changed after input loading")

    result = {}
    total_values = 0
    for fixture in fixtures["objects"]:
        ndim = _require_int(fixture["ndim"], f"{fixture['id']}.ndim", 2, MAX_DIMENSIONS)
        kind = fixture["kind"]
        if kind not in ("tbl", "list"):
            _fail(f"{fixture['id']}.kind must be tbl or list")
        names = fixture["list_names"]
        if kind == "tbl":
            if names is not None or fixture["ptype"] is not None:
                _fail(f"{fixture['id']} table root has list-only metadata")
        elif names is not None:
            _require_array(names, f"{fixture['id']}.list_names")
            if len(names) != len(fixture["members"]):
                _fail(f"{fixture['id']} list names do not match member count")
            for value in names:
                if value is not None:
                    _require_str(value, f"{fixture['id']} list name")
        elif kind != "list":
            _fail(f"{fixture['id']} has invalid list names")

        ptype = None
        if kind == "list":
            if fixture["ptype"] is None:
                _fail(f"{fixture['id']} list requires a zero-row prototype")
            ptype = _canonical_fixture_table(
                fixture["ptype"],
                ndim,
                script_dir,
                corpus_bytes,
                f"{fixture['id']}.ptype",
            )
            if any(column["data"] for column in ptype["columns"]):
                _fail(f"{fixture['id']} ptype must have zero rows")
        members = [
            _canonical_fixture_table(
                table, ndim, script_dir, corpus_bytes, f"{fixture['id']}.members[{i}]"
            )
            for i, table in enumerate(
                _require_array(fixture["members"], f"{fixture['id']}.members")
            )
        ]
        if kind == "tbl" and len(members) != 1:
            _fail(f"{fixture['id']} table must declare exactly one member")
        if kind == "list":
            for member in members:
                if any(
                    column["storage"] not in ("integer", "double")
                    for column in member["columns"]
                ):
                    _fail(f"{fixture['id']} list member has invalid storage")
        total_values += sum(
            len(column["data"]) for table in members for column in table["columns"]
        )
        if ptype is not None:
            total_values += sum(len(column["data"]) for column in ptype["columns"])
        if total_values > MAX_EXPECTED_VALUES:
            _fail("expanded fixture corpus exceeds the strict in-memory bound")
        result[fixture["id"]] = {
            "kind": kind,
            "ndim": ndim,
            "list_names": names.copy() if type(names) is list else None,
            "ptype": ptype,
            "members": members,
        }
        _validate_canonical_object(result[fixture["id"]])
    return result


def _validate_canonical_table(table, ndim, label):
    _require_keys(table, ("columns", "dim_names", "dim_units"), label=label)
    _validate_metadata(table["dim_names"], ndim, f"{label}.dim_names", unique=True)
    _validate_metadata(table["dim_units"], ndim, f"{label}.dim_units")
    columns = _require_array(table["columns"], f"{label}.columns")
    expected = [f"location_{axis}" for axis in range(1, ndim + 1)] + ["value"]
    if [column.get("name") for column in columns] != expected:
        _fail(f"{label} columns are not canonical")
    lengths = set()
    for column in columns:
        _require_keys(column, ("name", "storage", "data"), label=f"{label}.column")
        if column["storage"] not in ("integer", "double"):
            _fail(f"{label} column has invalid storage type")
        data = _require_array(column["data"], f"{label}.{column['name']}.data")
        for i, token in enumerate(data):
            _validate_fixture_token(
                token, column["storage"], f"{label}.{column['name']}[{i}]"
            )
        lengths.add(len(data))
    if len(lengths) != 1:
        _fail(f"{label} column lengths differ")
    return lengths.pop() if lengths else 0


def _validate_canonical_object(obj):
    _require_keys(
        obj,
        ("kind", "ndim", "list_names", "ptype", "members"),
        label="canonical object",
    )
    kind = obj["kind"]
    if kind not in ("tbl", "list"):
        _fail("canonical object kind must be tbl or list")
    ndim = _require_int(obj["ndim"], "canonical object dimensions", 2, MAX_DIMENSIONS)
    members = _require_array(obj["members"], "canonical object members")
    if kind == "tbl":
        if (
            obj["list_names"] is not None
            or obj["ptype"] is not None
            or len(members) != 1
        ):
            _fail("canonical table root has invalid list fields")
    else:
        names = obj["list_names"]
        if names is not None:
            _require_array(names, "canonical list names")
            if len(names) != len(members):
                _fail("canonical list names do not match member count")
            for name in names:
                if name is not None:
                    _require_str(name, "canonical member name")
        if obj["ptype"] is None:
            _fail("canonical list must have a ptype")
        if _validate_canonical_table(obj["ptype"], ndim, "canonical ptype") != 0:
            _fail("canonical ptype must have zero rows")
    total = 0
    for i, member in enumerate(members):
        total += _validate_canonical_table(member, ndim, f"canonical member {i}")
        if total > MAX_EXPECTED_VALUES:
            _fail("canonical object exceeds the in-memory point bound")
    return obj


def exact_equal(left, right):
    """Compare recursively without Python's bool/int or signed-zero shortcuts."""
    if type(left) is not type(right):
        return False
    if type(left) is dict:
        return left.keys() == right.keys() and all(
            exact_equal(left[key], right[key]) for key in left
        )
    if type(left) is list:
        return len(left) == len(right) and all(
            exact_equal(a, b) for a, b in zip(left, right)
        )
    if type(left) is float:
        return struct.pack(">d", left) == struct.pack(">d", right)
    return left == right


def _parse_uint(token, field, allow_zero=True):
    if type(token) is not str or not _UNSIGNED.fullmatch(token):
        _fail(f"Invalid unsigned integer in {field}")
    value = int(token)
    if value > MAX_INTEGER or (not allow_zero and value == 0):
        _fail(f"Unsigned integer outside supported range in {field}")
    return value


def _parse_index(token, field, allow_zero=False):
    return _parse_uint(token, field, allow_zero=allow_zero)


def _parse_flag(token, field):
    if token not in ("0", "1"):
        _fail(f"Invalid flag in {field}")
    return token == "1"


def _parse_integer_token(token, field):
    if token == "NA":
        return {"state": "na"}
    if type(token) is not str or not _INTEGER.fullmatch(token):
        _fail(f"Invalid integer token in {field}")
    value = int(token)
    if value < MIN_INTEGER or value > MAX_INTEGER:
        _fail(f"Integer outside supported range in {field}")
    return {"state": "number", "number": value}


def _parse_double_token(token, field):
    special = {
        "NA": {"state": "na"},
        "NaN": {"state": "nan"},
        "Inf": {"state": "+inf"},
        "-Inf": {"state": "-inf"},
        "-0": {"state": "number", "hex": "8000000000000000"},
    }
    if token in special:
        return special[token].copy()
    if type(token) is not str or not _DOUBLE.fullmatch(token):
        _fail(f"Invalid numeric token in {field}")
    try:
        value = float(token)
    except (OverflowError, ValueError) as exc:
        raise ValueError(f"Invalid numeric token in {field}") from exc
    if not math.isfinite(value):
        _fail(f"Double outside finite range in {field}")
    significand = re.split(r"[eE]", token, maxsplit=1)[0]
    if value == 0.0 and re.search(r"[1-9]", significand):
        _fail(f"Nonzero double underflows to zero in {field}")
    if value == 0.0 and token.startswith("-"):
        value = -0.0
    return {"state": "number", "hex": struct.pack(">d", value).hex()}


def _parse_numeric_token(token, storage, field):
    if storage == "integer":
        return _parse_integer_token(token, field)
    if storage == "double":
        return _parse_double_token(token, field)
    _fail(f"Invalid declared storage type in {field}")


def _format_numeric_token(token, storage, field):
    canonical = _validate_fixture_token(token, storage, field)
    state = canonical["state"]
    if state == "na":
        return "NA"
    if state == "nan":
        return "NaN"
    if state == "+inf":
        return "Inf"
    if state == "-inf":
        return "-Inf"
    if storage == "integer":
        return str(canonical["number"])
    value = _decode_hex64(canonical["hex"], field)
    if value == 0.0:
        return "-0" if math.copysign(1.0, value) < 0 else "0"
    return format(value, ".17g")


def _table_rows(obj, table_name):
    if table_name == "Members":
        return MEMBER_COLUMNS
    if table_name == "Axes":
        return AXIS_COLUMNS
    return (
        "member_index",
        "point_index",
        *[f"location_{i}" for i in range(1, obj["ndim"] + 1)],
        "value",
    )


def _object_tables(obj):
    obj = _validate_canonical_object(obj)
    ndim = obj["ndim"]
    descriptors = []
    if obj["kind"] == "list":
        descriptors.append((0, obj["ptype"], None, True))
    for index, table in enumerate(obj["members"], start=1):
        name = (
            None
            if obj["kind"] == "tbl" or obj["list_names"] is None
            else obj["list_names"][index - 1]
        )
        missing = obj["kind"] == "tbl" or (obj["list_names"] is None) or name is None
        descriptors.append((index, table, name, missing))

    members = []
    axes = []
    points = []
    for index, table, member_name, missing_name in descriptors:
        columns = table["columns"]
        n_points = len(columns[0]["data"])
        members.append(
            [
                str(index),
                "" if missing_name else member_name,
                "1" if missing_name else "0",
                "0" if table["dim_names"] is None else "1",
                "0" if table["dim_units"] is None else "1",
                columns[-1]["storage"],
                str(n_points),
            ]
        )
        for axis in range(ndim):
            dim_names = table["dim_names"]
            dim_units = table["dim_units"]
            axes.append(
                [
                    str(index),
                    str(axis + 1),
                    columns[axis]["storage"],
                    ""
                    if dim_names is None or dim_names[axis] is None
                    else dim_names[axis],
                    ""
                    if dim_units is None or dim_units[axis] is None
                    else dim_units[axis],
                ]
            )
        if index == 0:
            continue
        for point in range(n_points):
            row = [str(index), str(point + 1)]
            row.extend(
                _format_numeric_token(
                    column["data"][point],
                    column["storage"],
                    f"{column['name']}[{point}]",
                )
                for column in columns
            )
            points.append(row)
    return {"Members": members, "Axes": axes, "Points": points}


def _csv_bytes(rows, delimiter):
    import csv
    import io

    stream = io.StringIO(newline="")
    writer = csv.writer(
        stream,
        delimiter=delimiter,
        quoting=csv.QUOTE_ALL,
        lineterminator="\n",
        strict=True,
    )
    for row in rows:
        writer.writerow(row)
    data = stream.getvalue().encode("utf-8", errors="strict")
    if len(data) > DEFAULT_MAX_BYTES:
        _fail("serialized payload exceeds the independent writer byte limit")
    return data


def _parse_delimited(data, delimiter, field):
    if b"\x00" in data:
        _fail(f"Delimited payload contains NUL bytes: {field}")
    if data.startswith(b"\xef\xbb\xbf"):
        data = data[3:]
    try:
        text = data.decode("utf-8", errors="strict")
    except UnicodeDecodeError as exc:
        raise ValueError(f"Delimited payload is not valid UTF-8: {field}") from exc
    if delimiter not in (",", "\t"):
        _fail("Unsupported field delimiter")

    rows = []
    row = []
    field_chars = []
    mode = "start"
    touched = False
    i = 0
    length = len(text)

    def finish_field():
        nonlocal field_chars, row
        row.append("".join(field_chars))
        field_chars = []

    def finish_row():
        nonlocal row, touched
        rows.append(row)
        row = []
        touched = False

    while i < length:
        char = text[i]
        if mode == "quoted":
            if char == '"':
                if i + 1 < length and text[i + 1] == '"':
                    field_chars.append('"')
                    i += 2
                    touched = True
                    continue
                mode = "after_quote"
                i += 1
                touched = True
                continue
            field_chars.append(char)
            i += 1
            touched = True
            continue

        if mode == "after_quote":
            if char == delimiter:
                finish_field()
                mode = "start"
                i += 1
                touched = True
                continue
            if char == "\n":
                finish_field()
                finish_row()
                mode = "start"
                i += 1
                continue
            if char == "\r":
                if i + 1 >= length or text[i + 1] != "\n":
                    _fail(f"Bare carriage return outside quoted field in {field}")
                finish_field()
                finish_row()
                mode = "start"
                i += 2
                continue
            _fail(f"Unexpected character after closing quote in {field}")

        if char == '"':
            if mode != "start":
                _fail(f"Unexpected quote inside unquoted field in {field}")
            mode = "quoted"
            touched = True
            i += 1
            continue
        if char == delimiter:
            finish_field()
            mode = "start"
            touched = True
            i += 1
            continue
        if char == "\n":
            finish_field()
            finish_row()
            mode = "start"
            i += 1
            continue
        if char == "\r":
            if i + 1 >= length or text[i + 1] != "\n":
                _fail(f"Bare carriage return outside quoted field in {field}")
            finish_field()
            finish_row()
            mode = "start"
            i += 2
            continue
        field_chars.append(char)
        mode = "unquoted"
        touched = True
        i += 1

    if mode == "quoted":
        _fail(f"Delimited payload has an unterminated quoted field: {field}")
    if touched or mode != "start" or row or field_chars:
        finish_field()
        finish_row()
    if not rows:
        _fail(f"Delimited payload has no header: {field}")
    return rows


def _manifest_bytes(fields):
    lines = [f"{name}: {fields[name]}" for name in MANIFEST_FIELDS]
    return ("\n".join(lines) + "\n").encode("ascii")


def _manifest_from_fields(data):
    if not data or b"\x00" in data:
        _fail("Bundle manifest is empty or malformed")
    if any(byte > 127 for byte in data):
        _fail("Bundle manifest must contain ASCII fields")
    try:
        text = data.decode("ascii")
    except UnicodeDecodeError as exc:
        raise ValueError("Bundle manifest must contain ASCII fields") from exc
    # R's read.dcf accepts all three ASCII newline forms, folds indented
    # continuation lines, and trims ASCII whitespace around field values.
    # The public reader asks for `all = TRUE`, then requires exactly one
    # record and an exact field set. Match those DCF semantics here instead
    # of imposing the writer's canonical `Key: value` spelling on readers.
    lines = re.split(r"\r\n|\n|\r", text)
    dcf_space = " \t\v\f"

    def blank_line(line):
        return not line or all(char in dcf_space for char in line)

    while lines and blank_line(lines[0]):
        lines.pop(0)
    while lines and blank_line(lines[-1]):
        lines.pop()
    if not lines:
        _fail("Bundle manifest is empty or malformed")

    records = []
    current_record = []
    for line in lines:
        if blank_line(line):
            if current_record:
                records.append(current_record)
                current_record = []
            continue
        current_record.append(line)
    if current_record:
        records.append(current_record)
    if len(records) != 1:
        _fail("Bundle manifest is not a single fixed DCF record")

    fields = {}
    current_key = None
    for line in records[0]:
        if line[0] in dcf_space:
            if current_key is None:
                _fail("Bundle manifest has a continuation before its first field")
            continuation = line.lstrip(dcf_space).rstrip(dcf_space)
            if continuation:
                if fields[current_key]:
                    fields[current_key] += "\n" + continuation
                else:
                    fields[current_key] = continuation
            continue

        if ":" not in line:
            _fail("Bundle manifest has a malformed field line")
        key, value = line.split(":", 1)
        if not _FIELD_NAME.fullmatch(key):
            _fail("Bundle manifest has an invalid field name")
        if key in fields:
            _fail("Bundle manifest has duplicate fields")
        fields[key] = value.strip(dcf_space)
        current_key = key
    if set(fields) != set(MANIFEST_FIELDS):
        _fail("Bundle manifest has missing or unknown fields")
    if fields["Format"] != FORMAT:
        _fail("Unsupported bundle format")
    if fields["Version"] != VERSION:
        _fail("Unsupported measure-nd bundle version")
    if fields["Delimiter"] not in ("csv", "tsv"):
        _fail("Unsupported bundle delimiter")
    if fields["Object-Type"] not in ("tbl", "list"):
        _fail("Unsupported nD object type")
    if fields["List-Names"] not in ("absent", "present"):
        _fail("Invalid List-Names manifest value")
    ndim = _parse_uint(fields["Dimensions"], "Dimensions", allow_zero=False)
    if ndim < 2 or ndim > MAX_DIMENSIONS:
        _fail("Dimensions must be between 2 and 1024")
    if fields["Object-Type"] == "tbl" and fields["List-Names"] != "absent":
        _fail("A tbl bundle must declare List-Names: absent")
    fields["Dimensions"] = ndim
    for table in ("Members", "Axes", "Points"):
        rows_name = f"{table}-Rows"
        columns_name = f"{table}-Columns"
        fields[rows_name] = _parse_uint(fields[rows_name], rows_name)
        fields[columns_name] = _parse_uint(
            fields[columns_name], columns_name, allow_zero=False
        )
        expected_columns = (
            7 if table == "Members" else 5 if table == "Axes" else ndim + 3
        )
        if fields[columns_name] != expected_columns:
            _fail(f"Bundle manifest has an invalid field count for {table}")
        hash_field = f"{table}-SHA256"
        if type(fields[hash_field]) is not str or not _SHA256.fullmatch(
            fields[hash_field]
        ):
            _fail(f"Bundle manifest has an invalid SHA256 field for {table}")
    if fields["Object-Type"] == "tbl" and fields["Members-Rows"] != 1:
        _fail("A tbl bundle must have one member descriptor")
    if fields["Object-Type"] == "list" and fields["Members-Rows"] < 1:
        _fail("A list bundle must have a prototype descriptor")
    return fields


def _read_bundle_member(directory, basename, remaining):
    path = directory / basename
    if remaining < 0:
        _fail("Bundle exceeds max_bytes while reading")
    limit = remaining
    # A zero-byte remainder still performs the format's one-byte overflow probe.
    stream, _size = _open_regular(path)
    with stream:
        raw = stream.read(limit + 1)
    if len(raw) > limit:
        _fail("Bundle exceeds max_bytes while reading")
    return raw


def _parse_payload(raw, table, delimiter, manifest, ndim):
    rows = _parse_delimited(raw, delimiter, f"{table} payload")
    expected_header = (
        list(MEMBER_COLUMNS)
        if table == "Members"
        else list(AXIS_COLUMNS)
        if table == "Axes"
        else ["member_index", "point_index"]
        + [f"location_{axis}" for axis in range(1, ndim + 1)]
        + ["value"]
    )
    if rows[0] != expected_header:
        _fail(f"Bundle payload has unexpected columns for {table}")
    if len(rows) - 1 != manifest[f"{table}-Rows"]:
        _fail(f"Bundle payload row count does not match the manifest for {table}")
    if len(rows[0]) != manifest[f"{table}-Columns"]:
        _fail(f"Bundle payload column count does not match the manifest for {table}")
    for row in rows[1:]:
        if len(row) != len(expected_header):
            _fail(f"Bundle payload has an invalid row width for {table}")
    return rows[1:]


def _blank_to_missing(values):
    return [None if value == "" else value for value in values]


def _validate_read_metadata(values, field, unique=False):
    seen = set()
    for value in values:
        if value is None:
            continue
        _require_str(value, field)
        if _r_metadata_blank(value):
            _fail(f"Known {field} entries must be nonblank")
        if unique and value in seen:
            _fail(f"Known {field} entries must be unique")
        seen.add(value)


def _build_read_object(member_rows, axis_rows, point_rows, manifest, ndim):
    is_list = manifest["Object-Type"] == "list"
    members = []
    by_member = {}
    for row_number, row in enumerate(member_rows, start=1):
        index = _parse_index(
            row[0], f"members.member_index row {row_number}", allow_zero=True
        )
        missing_name = _parse_flag(row[2], "members.member_name_missing")
        names_present = _parse_flag(row[3], "members.dim_names_present")
        units_present = _parse_flag(row[4], "members.dim_units_present")
        value_type = row[5]
        point_count = _parse_uint(row[6], "members.point_count")
        if value_type not in ("integer", "double"):
            _fail("members.value_type must be integer or double")
        if missing_name and row[1] != "":
            _fail("Missing member names must have blank member_name cells")
        descriptor = {
            "index": index,
            "member_name": None if missing_name else row[1],
            "member_name_missing": missing_name,
            "dim_names_present": names_present,
            "dim_units_present": units_present,
            "value_type": value_type,
            "point_count": point_count,
        }
        if index in by_member:
            _fail("members table has duplicate member indices")
        by_member[index] = descriptor
        members.append(descriptor)

    if is_list:
        if (
            0 not in by_member
            or len([item for item in members if item["index"] == 0]) != 1
        ):
            _fail("list bundle must contain one prototype descriptor at index 0")
        prototype = by_member[0]
        if (
            not prototype["member_name_missing"]
            or member_rows[members.index(prototype)][1] != ""
            or prototype["point_count"] != 0
        ):
            _fail("invalid list prototype descriptor")
        real_indices = sorted(index for index in by_member if index > 0)
        if real_indices != list(range(1, len(real_indices) + 1)):
            _fail("List member indices must be contiguous from 1")
        if len(members) != len(real_indices) + 1:
            _fail("list bundle contains an invalid prototype or member descriptor")
        descriptor_order = [prototype] + [by_member[index] for index in real_indices]
        if manifest["List-Names"] == "absent":
            if any(
                not item["member_name_missing"] or item["member_name"] is not None
                for item in descriptor_order[1:]
            ):
                _fail("An unnamed list must have missing blank member names")
            list_names = None
        else:
            list_names = [item["member_name"] for item in descriptor_order[1:]]
    else:
        if (
            len(members) != 1
            or members[0]["index"] != 1
            or not members[0]["member_name_missing"]
            or member_rows[0][1] != ""
        ):
            _fail("A tbl bundle must have one unnamed member descriptor at index 1")
        descriptor_order = members
        list_names = None
        prototype = None

    expected_descriptor_indices = {item["index"] for item in descriptor_order}
    axes_by_member = {index: {} for index in expected_descriptor_indices}
    for row_number, row in enumerate(axis_rows, start=1):
        member_index = _parse_index(
            row[0], f"axes.member_index row {row_number}", allow_zero=True
        )
        axis_index = _parse_index(row[1], f"axes.axis_index row {row_number}")
        if member_index not in axes_by_member:
            _fail("Axes table contains an orphan member reference")
        if axis_index < 1 or axis_index > ndim:
            _fail("Axes table contains an invalid axis index")
        if axis_index in axes_by_member[member_index]:
            _fail("Axes table contains duplicate axis keys")
        if row[2] not in ("integer", "double"):
            _fail("axes.storage_type must be integer or double")
        axes_by_member[member_index][axis_index] = (row[2], row[3], row[4])
    for index, axes in axes_by_member.items():
        if set(axes) != set(range(1, ndim + 1)):
            _fail("Axes table has missing axis references")

    descriptor_axis_data = {}
    for descriptor in descriptor_order:
        index = descriptor["index"]
        axes = [axes_by_member[index][axis] for axis in range(1, ndim + 1)]
        dim_names_raw = [axis[1] for axis in axes]
        dim_units_raw = [axis[2] for axis in axes]
        if not descriptor["dim_names_present"] and any(
            value != "" for value in dim_names_raw
        ):
            _fail("Absent dim_names metadata must have blank axes.axis_name cells")
        if not descriptor["dim_units_present"] and any(
            value != "" for value in dim_units_raw
        ):
            _fail("Absent dim_units metadata must have blank axes.axis_unit cells")
        dim_names = (
            _blank_to_missing(dim_names_raw)
            if descriptor["dim_names_present"]
            else None
        )
        dim_units = (
            _blank_to_missing(dim_units_raw)
            if descriptor["dim_units_present"]
            else None
        )
        if dim_names is not None:
            _validate_read_metadata(dim_names, "dim_names", unique=True)
        if dim_units is not None:
            _validate_read_metadata(dim_units, "dim_units")
        descriptor_axis_data[index] = {
            "storage": [axis[0] for axis in axes],
            "dim_names": dim_names,
            "dim_units": dim_units,
        }

    real_indices = [item["index"] for item in descriptor_order if item["index"] > 0]
    point_groups = {index: [] for index in real_indices}
    parsed_point_indices = {index: [] for index in real_indices}
    for row_number, row in enumerate(point_rows, start=1):
        member_index = _parse_index(row[0], f"points.member_index row {row_number}")
        point_index = _parse_index(row[1], f"points.point_index row {row_number}")
        if member_index not in point_groups:
            _fail("Points table contains orphan or prototype point references")
        parsed_point_indices[member_index].append(point_index)
        point_groups[member_index].append(row)
    if sum(
        item["point_count"] for item in descriptor_order if item["index"] > 0
    ) != len(point_rows):
        _fail("Member point_count values do not sum to the points row count")
    for descriptor in descriptor_order:
        index = descriptor["index"]
        if index == 0:
            continue
        group = point_groups[index]
        if len(group) != descriptor["point_count"]:
            _fail("members.point_count does not match point references")
        if sorted(parsed_point_indices[index]) != list(range(1, len(group) + 1)):
            _fail("Point indices must be unique and contiguous within each member")
        group.sort(key=lambda row: _parse_index(row[1], "points.point_index"))

    parsed_point_data = {}
    for descriptor in descriptor_order:
        index = descriptor["index"]
        if index == 0:
            continue
        storage_types = [
            *descriptor_axis_data[index]["storage"],
            descriptor["value_type"],
        ]
        columns = [[] for _ in storage_types]
        for point_number, row in enumerate(point_groups[index]):
            for column_index, storage in enumerate(storage_types):
                name = (
                    f"location_{column_index + 1}" if column_index < ndim else "value"
                )
                columns[column_index].append(
                    _parse_numeric_token(
                        row[2 + column_index],
                        storage,
                        f"points.{name} for member {index} row {point_number + 1}",
                    )
                )
        parsed_point_data[index] = columns

    def build_table(descriptor):
        index = descriptor["index"]
        axis_info = descriptor_axis_data[index]
        all_storage = [*axis_info["storage"], descriptor["value_type"]]
        columns = []
        for column_index, storage in enumerate(all_storage):
            name = f"location_{column_index + 1}" if column_index < ndim else "value"
            data = [] if index == 0 else parsed_point_data[index][column_index]
            columns.append({"name": name, "storage": storage, "data": data})
        return {
            "columns": columns,
            "dim_names": axis_info["dim_names"],
            "dim_units": axis_info["dim_units"],
        }

    if is_list:
        ptype = build_table(prototype)
        output_members = [build_table(item) for item in descriptor_order[1:]]
        result = {
            "kind": "list",
            "ndim": ndim,
            "list_names": list_names,
            "ptype": ptype,
            "members": output_members,
        }
    else:
        result = {
            "kind": "tbl",
            "ndim": ndim,
            "list_names": None,
            "ptype": None,
            "members": [build_table(descriptor_order[0])],
        }
    _validate_canonical_object(result)
    return result


def read_bundle(path, max_bytes=DEFAULT_MAX_BYTES):
    """Read a measure-nd bundle with strict schema, state, and byte checks."""
    max_bytes = _check_limit(max_bytes)
    path = Path(path)
    if not path.is_dir():
        _fail(f"path must name an existing bundle directory: {path}")
    directory = path.resolve(strict=True)
    manifest_bytes = _read_regular_bytes(directory / "manifest.dcf", max_bytes)
    remaining = max_bytes - len(manifest_bytes)
    manifest = _manifest_from_fields(manifest_bytes)
    delimiter = "," if manifest["Delimiter"] == "csv" else "\t"
    raw_payloads = {}
    for table in ("Members", "Axes", "Points"):
        extension = "csv" if manifest["Delimiter"] == "csv" else "tsv"
        basename = f"{table.lower()}.{extension}"
        payload = _read_bundle_member(directory, basename, remaining)
        remaining -= len(payload)
        digest = hashlib.sha256(payload).hexdigest()
        if digest != manifest[f"{table}-SHA256"]:
            _fail(f"Bundle payload hash does not match the manifest for {basename}")
        raw_payloads[table] = payload

    ndim = manifest["Dimensions"]
    parsed = {
        table: _parse_payload(raw_payloads[table], table, delimiter, manifest, ndim)
        for table in ("Members", "Axes", "Points")
    }
    return _build_read_object(
        parsed["Members"], parsed["Axes"], parsed["Points"], manifest, ndim
    )


def _table_manifest_values(tables):
    values = {}
    for table, rows in tables.items():
        values[f"{table}-Rows"] = str(len(rows) - 1)
        values[f"{table}-Columns"] = str(len(rows[0]))
    return values


def _copy_exclusive(source, destination):
    with open(source, "rb") as src:
        flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
        flags |= getattr(os, "O_NOFOLLOW", 0)
        descriptor = os.open(destination, flags, 0o666)
        with os.fdopen(descriptor, "wb") as dst:
            shutil.copyfileobj(src, dst, length=1024 * 1024)
            dst.flush()
            os.fsync(dst.fileno())


def write_bundle(obj, path, delimiter):
    """Write a strict Python-authored reverse fixture into a new bundle."""
    obj = _validate_canonical_object(obj)
    if delimiter not in ("csv", "tsv"):
        _fail("delimiter must be csv or tsv")
    destination = Path(path)
    if not destination.name or destination.name in (".", ".."):
        _fail("path must name a new bundle directory")
    if destination.exists() or destination.is_symlink():
        _fail(f"path already exists: {destination}")
    parent = destination.parent.resolve(strict=True)
    if not parent.is_dir():
        _fail("bundle parent must be an existing directory")
    destination = parent / destination.name
    field_delimiter = "," if delimiter == "csv" else "\t"
    names = {
        "Members": MEMBER_COLUMNS,
        "Axes": AXIS_COLUMNS,
        "Points": _table_rows(obj, "Points"),
    }
    data_tables = _object_tables(obj)
    payloads = {}
    tables = {}
    for table in ("Members", "Axes", "Points"):
        rows = [list(names[table]), *data_tables[table]]
        payload = _csv_bytes(rows, field_delimiter)
        extension = "csv" if delimiter == "csv" else "tsv"
        payloads[f"{table.lower()}.{extension}"] = payload
        tables[table] = rows

    fields = {
        "Format": FORMAT,
        "Version": VERSION,
        "Delimiter": delimiter,
        "Object-Type": obj["kind"],
        "Dimensions": str(obj["ndim"]),
        "List-Names": "absent"
        if obj["kind"] == "tbl" or obj["list_names"] is None
        else "present",
    }
    for table in ("Members", "Axes", "Points"):
        fields[f"{table}-Rows"] = str(len(tables[table]) - 1)
        fields[f"{table}-Columns"] = str(len(tables[table][0]))
        fields[f"{table}-SHA256"] = hashlib.sha256(
            payloads[f"{table.lower()}.{delimiter}"]
        ).hexdigest()
    fields = {key: fields[key] for key in MANIFEST_FIELDS}

    stage = Path(tempfile.mkdtemp(prefix=f".{destination.name}-stage-", dir=parent))
    try:
        for basename, payload in payloads.items():
            with open(stage / basename, "xb") as stream:
                stream.write(payload)
                stream.flush()
                os.fsync(stream.fileno())
        with open(stage / "manifest.dcf", "xb") as stream:
            stream.write(_manifest_bytes(fields))
            stream.flush()
            os.fsync(stream.fileno())
        total_size = sum(
            (stage / basename).stat().st_size
            for basename in ("manifest.dcf", *payloads)
        )
        staged = read_bundle(stage, max_bytes=max(1, total_size))
        if not exact_equal(staged, obj):
            _fail("staged Python bundle did not preserve its object")

        try:
            os.mkdir(destination)
        except FileExistsError as exc:
            raise ValueError("destination appeared during writing") from exc
        for basename in payloads:
            _copy_exclusive(stage / basename, destination / basename)
        _copy_exclusive(stage / "manifest.dcf", destination / "manifest.dcf")
    finally:
        shutil.rmtree(stage, ignore_errors=True)
    return destination
