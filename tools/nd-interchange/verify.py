"""Verify installed-R nD interchange against fixed binary truth, using stdlib only."""

import argparse
import datetime
from pathlib import Path, PurePosixPath, PureWindowsPath

from nd_profile import (
    exact_equal,
    expected_objects,
    load_inputs,
    read_bundle,
    read_json,
    sha256_file,
    write_json_exclusive,
)

PROFILE = "measure-nd-interchange/1"
HERE = Path(__file__).resolve().parent
PRODUCER_FILES = ("observations.json", "runtime.json", "run-status.json")
SOURCE_FILES = (*PRODUCER_FILES, "independent-python-receipt.json")


def require(condition, message):
    if not condition:
        raise ValueError(message)


def same(actual, expected, label):
    require(exact_equal(actual, expected), f"{label} differs from the declared truth")


def keys(value, names, label):
    require(type(value) is dict and set(value) == set(names), f"Invalid {label} fields")


def utc_now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def hashes(root, names):
    return {name: sha256_file(root / name) for name in names}


def forward_hashes(root, declaration):
    return {
        row["id"]: bundle_files(root / "bundles" / row["id"])
        for row in declaration["forward_bundles"]
    }


def runtime_check(value):
    keys(
        value,
        (
            "profile",
            "R",
            "installed_package",
            "tool_checkout_head",
            "created_utc",
            "session_info",
            "dependency_versions",
        ),
        "runtime",
    )
    require(value["profile"] == PROFILE, "Wrong runtime profile")
    require(
        type(value["R"]) is str and value["R"].startswith("R version "),
        "Invalid R version",
    )
    package = value["installed_package"]
    keys(
        package,
        ("path", "namespace_path", "version", "built", "remote_sha"),
        "installed package",
    )
    path = package["path"]
    require(
        type(path) is str
        and (PurePosixPath(path).is_absolute() or PureWindowsPath(path).is_absolute()),
        "Invalid installed path",
    )
    same(package["namespace_path"], package["path"], "Namespace path")
    require(
        type(package["version"]) is str and bool(package["version"]),
        "Missing installed version",
    )
    for field in ("built", "remote_sha"):
        require(
            package[field] is None or type(package[field]) is str, f"Invalid {field}"
        )
    head = value["tool_checkout_head"]
    require(
        head is None
        or (
            type(head) is str
            and len(head) in {40, 64}
            and all(c in "0123456789abcdef" for c in head)
        ),
        "Invalid tool HEAD",
    )
    session = value["session_info"]
    require(
        type(session) is list and session and all(type(x) is str for x in session),
        "Invalid session info",
    )
    same(session[0], value["R"], "Session R")
    deps = value["dependency_versions"]
    require(
        type(deps) is dict
        and set(deps) >= {"measure", "vctrs", "jsonlite", "digest", "fs"},
        "Missing dependency versions",
    )
    require(
        all(type(x) is str and x for x in deps.values()), "Invalid dependency versions"
    )
    same(deps["measure"], package["version"], "Installed version")
    require(
        type(value["created_utc"]) is str and value["created_utc"],
        "Missing runtime time",
    )


def status_check(root, filename, phase):
    value = read_json(root / filename)
    keys(value, ("phase", "complete", "stage", "error", "updated_utc"), filename)
    same(value["phase"], phase, "Phase")
    require(
        value["complete"] is True
        and value["stage"] == "complete"
        and value["error"] is None,
        f"{phase} phase is incomplete",
    )
    require(
        type(value["updated_utc"]) is str and value["updated_utc"],
        "Missing status time",
    )


def producer(root, declaration, input_hashes, expected):
    status_check(root, "run-status.json", "produce")
    observed = read_json(root / "observations.json")
    runtime = read_json(root / "runtime.json")
    runtime_check(runtime)
    keys(
        observed,
        (
            "profile",
            "input_files_sha256",
            "installed_package",
            "R",
            "tool_checkout_head",
            "bundles",
        ),
        "observations",
    )
    same(observed["profile"], PROFILE, "Observation profile")
    same(observed["input_files_sha256"], input_hashes, "Observation inputs")
    for key in ("installed_package", "R", "tool_checkout_head"):
        same(observed[key], runtime[key], key)
    rows = observed["bundles"]
    declared = declaration["forward_bundles"]
    require(
        type(rows) is list and len(rows) == len(declared), "Wrong forward bundle count"
    )
    for row, spec in zip(rows, declared):
        keys(
            row, ("id", "object_id", "delimiter", "path", "observed"), "forward bundle"
        )
        for key in ("id", "object_id", "delimiter"):
            same(row[key], spec[key], key)
        same(row["path"], f"bundles/{spec['id']}", "Forward path")
        truth = expected[spec["object_id"]]
        same(row["observed"], truth, f"R read-back {spec['id']}")
        same(read_bundle(root / row["path"]), truth, f"Independent read {spec['id']}")
    return runtime


def bundle_files(path):
    require(not path.is_symlink() and path.is_dir(), "Invalid bundle directory")
    result = {}
    for name in (
        "manifest.dcf",
        "members.csv",
        "axes.csv",
        "points.csv",
        "members.tsv",
        "axes.tsv",
        "points.tsv",
    ):
        file = path / name
        if file.exists() or file.is_symlink():
            result[name] = sha256_file(file)
    # Missing manifest/payloads are intentional corruption cases, recorded as absent.
    return result


def mutation_index(root, declaration, input_hashes):
    index = read_json(root / "mutations.json")
    keys(
        index,
        (
            "profile",
            "input_files_sha256",
            "source_artifacts_sha256",
            "invalid_bundles",
            "valid_bundles",
        ),
        "mutations",
    )
    same(index["profile"], PROFILE, "Mutation profile")
    same(index["input_files_sha256"], input_hashes, "Mutation inputs")
    same(
        index["source_artifacts_sha256"], hashes(root, SOURCE_FILES), "Mutation sources"
    )
    invalid = index["invalid_bundles"]
    require(
        type(invalid) is list and len(invalid) == len(declaration["invalid_bundles"]),
        "Wrong mutation count",
    )
    for row, ident in zip(invalid, declaration["invalid_bundles"]):
        keys(row, ("id", "path", "files_sha256"), "invalid bundle")
        same(row["id"], ident, "Mutation id")
        same(row["path"], f"mutations/{ident}", "Mutation path")
        same(row["files_sha256"], bundle_files(root / row["path"]), "Mutation bytes")
    valid_specs = [(x, "reverse") for x in declaration["reverse_bundles"]]
    valid_specs += [(x, "reordered") for x in declaration["reordered_bundles"]]
    valid = index["valid_bundles"]
    require(
        type(valid) is list and len(valid) == len(valid_specs),
        "Wrong valid controls count",
    )
    for row, (spec, kind) in zip(valid, valid_specs):
        keys(row, ("id", "object_id", "kind", "path", "files_sha256"), "valid bundle")
        for key in ("id", "object_id"):
            same(row[key], spec[key], key)
        same(row["kind"], kind, "Control kind")
        same(row["path"], f"{kind}/{spec['id']}", "Control path")
        same(row["files_sha256"], bundle_files(root / row["path"]), "Control bytes")
    status_check(root, "mutation-status.json", "mutate")
    return index


def rejection_check(root, declaration, input_hashes, expected, runtime):
    index = mutation_index(root, declaration, input_hashes)
    for case in index["invalid_bundles"]:
        try:
            read_bundle(root / case["path"])
        except (ValueError, OSError):
            pass
        else:
            raise ValueError(f"Python accepted declared corruption {case['id']}")
    status_check(root, "rejection-status.json", "reject")
    result = read_json(root / "rejections.json")
    keys(
        result,
        (
            "profile",
            "input_files_sha256",
            "source_artifacts_sha256",
            "runtime",
            "valid_bundles",
            "invalid_bundles",
            "cross_format_rejections",
        ),
        "rejections",
    )
    same(result["profile"], PROFILE, "Rejection profile")
    same(result["input_files_sha256"], input_hashes, "Rejection inputs")
    same(
        result["source_artifacts_sha256"],
        hashes(root, (*SOURCE_FILES, "mutations.json")),
        "Rejection sources",
    )
    runtime_check(result["runtime"])
    for key in ("R", "installed_package", "dependency_versions"):
        same(result["runtime"][key], runtime[key], f"Rejection runtime {key}")
    valid = result["valid_bundles"]
    require(
        type(valid) is list and len(valid) == len(index["valid_bundles"]),
        "Wrong read controls count",
    )
    for row, spec in zip(valid, index["valid_bundles"]):
        keys(row, ("id", "object_id", "kind", "observed"), "read control")
        for key in ("id", "object_id", "kind"):
            same(row[key], spec[key], key)
        truth = expected[spec["object_id"]]
        same(row["observed"], truth, f"R control {spec['id']}")
        same(read_bundle(root / spec["path"]), truth, f"Python control {spec['id']}")
    for field, ids in (
        ("invalid_bundles", declaration["invalid_bundles"]),
        ("cross_format_rejections", declaration["cross_format_rejections"]),
    ):
        rows = result[field]
        require(type(rows) is list and len(rows) == len(ids), f"Wrong {field} count")
        for row, ident in zip(rows, ids):
            keys(row, ("id", "error", "message"), "rejection")
            same(row["id"], ident, "Rejection id")
            require(
                row["error"] is True
                and type(row["message"]) is str
                and row["message"].strip(),
                f"Missing actual error for {ident}",
            )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("--rejections", action="store_true")
    args = parser.parse_args()
    require(
        not args.output.is_symlink() and args.output.is_dir(),
        "OUTPUT must be a real existing directory",
    )
    root = args.output.resolve(strict=True)
    fixtures, declaration, input_hashes = load_inputs(HERE)
    expected = expected_objects(fixtures, HERE)
    runtime = producer(root, declaration, input_hashes, expected)
    bindings = hashes(root, PRODUCER_FILES)
    receipt = {
        "profile": PROFILE,
        "input_files_sha256": input_hashes,
        "source_artifacts_sha256": bindings,
        "installed_package": runtime["installed_package"],
        "forward_bundle_files_sha256": forward_hashes(root, declaration),
        "objects": len(expected),
        "forward_bundles": len(declaration["forward_bundles"]),
        "verified_utc": utc_now(),
    }
    if args.rejections:
        previous = read_json(root / "independent-python-receipt.json")
        for key in (
            "profile",
            "input_files_sha256",
            "source_artifacts_sha256",
            "installed_package",
            "forward_bundle_files_sha256",
            "objects",
            "forward_bundles",
        ):
            same(previous[key], receipt[key], f"Forward receipt {key}")
        rejection_check(root, declaration, input_hashes, expected, runtime)
        receipt["source_artifacts_sha256"] = hashes(
            root,
            (
                *SOURCE_FILES,
                "mutations.json",
                "rejections.json",
                "rejection-status.json",
            ),
        )
        receipt.update(valid_controls=4, invalid_bundles=49, cross_format_rejections=2)
        name = "independent-rejection-receipt.json"
    else:
        name = "independent-python-receipt.json"
    write_json_exclusive(root / name, receipt)
    print(
        f"Verified {len(expected)} objects / 30 forward bundles"
        + (" / 4 controls / 51 rejections" if args.rejections else "")
    )


if __name__ == "__main__":
    main()
