"""Independently check fixed nD API outputs using only the Python standard library."""

import hashlib
import itertools
import json
import math
import os
import platform
import re
import stat
import sys
from pathlib import Path

if not __debug__ or os.environ.get("PYTHONOPTIMIZE"):
    raise SystemExit(
        "Run without Python optimization: verification assertions must remain enabled."
    )

FROZEN = {
    "fixtures.json": "c5692fb3cc7cc69ad746046408779b07d5bed01d11a57d895ad33e648ad7115b",
    "declaration.json": "036bad1ac89c7c759de2b6ae8fc2430c0e0052bf80a92c77f196c2b2da0257ef",
}
PROFILE = "measure-nd-operations/1"


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON field: " + key)
        result[key] = value
    return result


def invalid_constant(value):
    raise ValueError("Non-JSON numeric token: " + value)


def read_json(path, max_bytes=32 * 1024 * 1024):
    info = path.lstat()
    assert stat.S_ISREG(info.st_mode) and info.st_size <= max_bytes, path
    with path.open("rb") as stream:
        raw = stream.read(max_bytes + 1)
    assert len(raw) <= max_bytes, path
    return json.loads(
        raw.decode("utf-8"),
        object_pairs_hook=unique_object,
        parse_constant=invalid_constant,
    ), hashlib.sha256(raw).hexdigest()


def signal(value):
    return (
        {"state": "na", "number": None}
        if value is None
        else {"state": "number", "number": float(value)}
    )


def result(fixture, rows, keep=None):
    keep = list(range(fixture["ndim"])) if keep is None else keep
    n = len(keep)
    return {
        "kind": "scalar" if n == 0 else "1d" if n == 1 else "nd",
        "ndim": n,
        "dim_names": [fixture["dim_names"][i] for i in keep] if n >= 2 else None,
        "dim_units": [fixture["dim_units"][i] for i in keep] if n >= 2 else None,
        "rows": [
            {"coordinates": [row[i] for i in keep], "value": signal(row[-1])}
            for row in rows
        ],
    }


def fold_expected(fixture, order, edited=False):
    # The declared first dimension varies fastest; no R sorting code is used.
    rows = sorted(
        fixture["rows"], key=lambda row: tuple(row[i - 1] for i in reversed(order))
    )
    if edited:
        rows = [
            row[:-1] + [None if row[-1] is None else 2 * row[-1] - 1] for row in rows
        ]
    return result(fixture, rows)


def slice_expected(fixture, spec):
    def selected(row):
        for selector in spec["selectors"]:
            value = row[selector["dimension"] - 1]
            if "values" in selector and value not in selector["values"]:
                return False
            if (
                "between" in selector
                and not selector["between"][0] <= value <= selector["between"][1]
            ):
                return False
        return True

    rows = [row for row in fixture["rows"] if selected(row)]
    keep = list(range(fixture["ndim"]))
    if spec["drop"]:
        keep = [i for i in keep if len({row[i] for row in rows}) != 1]
    return result(fixture, rows, keep)


def aggregate(values, fn, na_rm):
    if not na_rm and any(value is None for value in values):
        return signal(None)
    values = [value for value in values if value is not None]
    if fn == "sum":
        return signal(math.fsum(values))
    assert fn == "mean"
    if not values:
        return {"state": "nan", "number": None}
    return signal(math.fsum(values) / len(values))


def projection_expected(fixture, spec):
    keep = [i for i in range(fixture["ndim"]) if i + 1 not in spec["along"]]
    grouped = {}
    for row in fixture["rows"]:
        grouped.setdefault(tuple(row[i] for i in keep), []).append(row[-1])
    if not keep and not grouped:
        grouped[()] = []
    n = len(keep)
    return {
        "kind": "scalar" if n == 0 else "1d" if n == 1 else "nd",
        "ndim": n,
        "dim_names": [fixture["dim_names"][i] for i in keep] if n >= 2 else None,
        "dim_units": [fixture["dim_units"][i] for i in keep] if n >= 2 else None,
        "rows": [
            {
                "coordinates": list(key),
                "value": aggregate(grouped[key], spec["fn"], spec["na_rm"]),
            }
            for key in sorted(grouped)
        ],
    }


COUNTS = {"results": 0, "rows": 0, "coordinate_values": 0, "signal_values": 0}


def check_number(actual, expected, approximate=False):
    assert type(actual) in (int, float) and math.isfinite(actual), actual
    if approximate:
        assert math.isclose(actual, expected, abs_tol=1e-12, rel_tol=1e-12), (
            actual,
            expected,
        )
    else:
        assert float(actual) == float(expected), (actual, expected)


def check_result(actual, expected, approximate=False):
    assert set(actual) == set(expected), (set(actual), set(expected))
    for key in ["kind", "ndim", "dim_names", "dim_units"]:
        assert actual[key] == expected[key], (key, actual[key], expected[key])
    assert type(actual["ndim"]) is int
    assert isinstance(actual["rows"], list) and len(actual["rows"]) == len(
        expected["rows"]
    )
    COUNTS["results"] += 1
    for row, wanted in zip(actual["rows"], expected["rows"]):
        assert set(row) == {"coordinates", "value"}
        coordinates = row["coordinates"]
        assert isinstance(coordinates, list) and len(coordinates) == expected["ndim"]
        for value, reference in zip(coordinates, wanted["coordinates"]):
            check_number(value, reference)
            COUNTS["coordinate_values"] += 1
        value, reference = row["value"], wanted["value"]
        assert set(value) == {"state", "number"}
        assert value["state"] == reference["state"], (value, reference)
        if reference["state"] == "number":
            check_number(value["number"], reference["number"], approximate)
        else:
            assert value["number"] is None
        COUNTS["rows"] += 1
        COUNTS["signal_values"] += 1


def main():
    assert len(sys.argv) == 2, "Provide the qualification output root."
    output = Path(sys.argv[1])
    here = Path(__file__).resolve().parent
    inputs = here / "qualification-input"
    if not inputs.is_dir():
        # Public layout places the two fixed files directly beside the tools.
        inputs = here
    fixtures, fixture_sha = read_json(inputs / "fixtures.json")
    declaration, declaration_sha = read_json(inputs / "declaration.json")
    hashes = {"fixtures.json": fixture_sha, "declaration.json": declaration_sha}
    assert hashes == FROZEN, hashes
    assert declaration["profile"] == PROFILE
    observed, result_sha = read_json(output / "results.json")
    assert observed["profile"] == PROFILE and observed["input_files_sha256"] == FROZEN
    assert isinstance(observed["R"], str) and observed["R"]
    installed = observed["installed_measure"]
    assert isinstance(installed["path"], str) and installed["path"]
    assert installed["namespace_path"] == installed["path"]
    assert isinstance(installed["version"], str) and installed["version"]
    runtime, runtime_sha = read_json(output / "runtime.json")
    status, status_sha = read_json(output / "run-status.json")
    assert status["complete"] is True and status["stage"] == "complete"
    assert status["error"] is None
    assert isinstance(status["updated_utc"], str) and status["updated_utc"]
    assert isinstance(runtime["created_utc"], str) and runtime["created_utc"]
    session = runtime["session_info"]
    assert isinstance(session, list) and session and session[0] == observed["R"]
    assert all(isinstance(line, str) for line in session)
    versions = runtime["dependency_versions"]
    assert isinstance(versions, dict) and versions["measure"] == installed["version"]
    for dependency in (
        "measure",
        "recipes",
        "dplyr",
        "tibble",
        "vctrs",
        "jsonlite",
        "digest",
    ):
        assert isinstance(versions[dependency], str) and versions[dependency]
    head = observed["tool_checkout_head"]
    assert head is None or re.fullmatch("(?:[0-9a-f]{40}|[0-9a-f]{64})", head)
    fixture_map = {f["id"]: f for f in fixtures["fixtures"]}
    assert len(fixture_map) == len(fixtures["fixtures"])
    expected_fold = {}
    for cohort_index, cohort in enumerate(declaration["cohorts"], 1):
        ndim = fixture_map[cohort[0]]["ndim"]
        for order, variant in itertools.product(
            declaration["fold_orders"][str(ndim)], declaration["fold_variants"]
        ):
            ids = (
                list(reversed(cohort))
                if variant == "reverse_list"
                else cohort[:1]
                if variant == "first_member_subset"
                else cohort
            )
            expected_fold[(cohort_index, tuple(order), variant)] = ids
    assert len(observed["fold"]) == len(expected_fold)
    seen = set()
    for case in observed["fold"]:
        key = (case["cohort"], tuple(case["order"]), case["variant"])
        assert key in expected_fold and key not in seen, key
        seen.add(key)
        ids = expected_fold[key]
        assert [m["fixture_id"] for m in case["members"]] == ids
        for member in case["members"]:
            expected = fold_expected(
                fixture_map[member["fixture_id"]],
                case["order"],
                case["variant"] == "edited_values",
            )
            check_result(member["result"], expected)
    assert seen == set(expected_fold)
    for field, build in [
        ("slices", slice_expected),
        ("projections", projection_expected),
    ]:
        specifications = {s["id"]: s for s in declaration[field]}
        assert len(observed[field]) == len(specifications)
        seen = set()
        for case in observed[field]:
            name = case["id"]
            assert name in specifications and name not in seen
            seen.add(name)
            spec = specifications[name]
            check_result(
                case["result"],
                build(fixture_map[spec["fixture"]], spec),
                field == "projections",
            )
        assert seen == set(specifications)
    invalid = observed["invalid_cases"]
    assert len(invalid) == len(declaration["invalid_cases"])
    assert {case["id"] for case in invalid} == set(declaration["invalid_cases"])
    for case in invalid:
        assert case["error"] is True
        assert isinstance(case["message"], str) and case["message"].strip()
    selection = observed["list_selection"]["members"]
    assert [member["fixture_id"] for member in selection] == ["dad-a", "dad-b"]
    for member in selection:
        spec = {"selectors": [{"dimension": 2, "values": [254]}], "drop": False}
        check_result(
            member["result"], slice_expected(fixture_map[member["fixture_id"]], spec)
        )
    receipt = {
        "profile": PROFILE,
        "independent_reader": "Python standard library coordinate oracle/1",
        "python_version": platform.python_version(),
        "input_files_sha256": hashes,
        "results_sha256": result_sha,
        "runtime_sha256": runtime_sha,
        "run_status_sha256": status_sha,
        "counts": COUNTS,
        "fold_cases": len(expected_fold),
        "slice_cases": len(declaration["slices"]),
        "projection_cases": len(declaration["projections"]),
        "expected_rejections": len(invalid),
        "all_declared_cases_match": True,
        "scope": declaration["scope"],
    }
    with (output / "independent-python-receipt.json").open(
        "x", encoding="utf-8"
    ) as stream:
        json.dump(receipt, stream, indent=2, allow_nan=False)
        stream.write("\n")
    print(
        json.dumps(
            {
                k: v
                for k, v in receipt.items()
                if k
                in [
                    "counts",
                    "fold_cases",
                    "slice_cases",
                    "projection_cases",
                    "expected_rejections",
                    "all_declared_cases_match",
                ]
            },
            indent=2,
        )
    )


if __name__ == "__main__":
    main()
