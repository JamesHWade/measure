"""Create fixed corruption cases without overwriting or cleaning user files."""

import csv
import hashlib
import json
import shutil
import sys
from pathlib import Path

if not __debug__:
    raise SystemExit(
        "Run this mutator without Python optimization so its checks remain active."
    )

SCRIPT_DIR = Path(__file__).resolve().parent
PROFILE = "measure-record-interchange-qualification/1"
DECLARATION_PATH = SCRIPT_DIR / "qualification-declaration.json"
INPUT_MANIFEST_PATH = SCRIPT_DIR / "qualification-input-manifest.json"
DECLARATION = json.loads(DECLARATION_PATH.read_text(encoding="utf-8"))
INPUT_MANIFEST = json.loads(INPUT_MANIFEST_PATH.read_text(encoding="utf-8"))
CASE_OUTCOMES = (
    ("corrupt-numeric-byte", "reject"),
    ("missing-table", "reject"),
    ("unknown-version", "reject"),
    ("duplicate-index-rehashed", "reject"),
    ("fractional-source-row-rehashed", "reject"),
    ("reversed-physical-rows-rehashed", "restore"),
    ("partial-no-manifest", "reject"),
    ("symlink-member", "reject"),
)


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
    receipt_path = root / "public-R-receipt.json"
    if marker_path.is_symlink() or not marker_path.is_file():
        raise SystemExit("OUTPUT is missing its qualification-root ownership marker.")
    if receipt_path.is_symlink() or not receipt_path.is_file():
        raise SystemExit(
            "OUTPUT is missing the completed public R qualification receipt."
        )
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


def ensure_new(path: Path) -> None:
    if path.exists() or path.is_symlink():
        raise SystemExit(f"Refusing to overwrite an existing path: {path}")


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("Usage: python3 mutate.py OUTPUT")
    verify_inputs()
    root = owned_root(sys.argv[1])
    source = root / "from-csv-to-csv"
    if source.is_symlink() or not source.is_dir():
        raise SystemExit("OUTPUT is missing the fixed from-csv-to-csv bundle.")
    manifest_output = root / "mutations.json"
    destinations = [root / name for name, _ in CASE_OUTCOMES]
    for path in [manifest_output, *destinations]:
        ensure_new(path)

    cases = []

    def make(name: str, expect: str = "reject") -> Path:
        expected = dict(CASE_OUTCOMES)
        assert name in expected and expected[name] == expect
        path = root / name
        shutil.copytree(source, path)
        cases.append({"directory": name, "expect": expect})
        return path

    def rehash(path: Path, member: str) -> None:
        prefix = member.split(".")[0].title() + "-SHA256:"
        manifest = path / "manifest.dcf"
        lines = manifest.read_text(encoding="utf-8").splitlines()
        sha = digest(path / member)
        assert sum(line.startswith(prefix) for line in lines) == 1
        manifest.write_text(
            "\n".join(
                prefix + " " + sha if line.startswith(prefix) else line
                for line in lines
            )
            + "\n",
            encoding="utf-8",
        )

    def point_edit(name: str, transform, expect: str = "reject") -> None:
        path = make(name, expect)
        points = path / "points.csv"
        with points.open(encoding="utf-8", newline="") as stream:
            reader = csv.DictReader(stream, strict=True)
            fields = reader.fieldnames
            rows = list(reader)
        transform(rows)
        with points.open("w", encoding="utf-8", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=fields)
            writer.writeheader()
            writer.writerows(rows)
        rehash(path, "points.csv")

    path = make("corrupt-numeric-byte")
    data = (path / "points.csv").read_bytes()
    assert b"0.10000000000000001" in data
    (path / "points.csv").write_bytes(
        data.replace(b"0.10000000000000001", b"0.10000000000000002", 1)
    )

    path = make("missing-table")
    (path / "channels.csv").unlink()

    path = make("unknown-version")
    manifest = (path / "manifest.dcf").read_text(encoding="utf-8")
    assert "Version: 1\n" in manifest
    (path / "manifest.dcf").write_text(
        manifest.replace("Version: 1\n", "Version: 2\n"), encoding="utf-8"
    )

    point_edit(
        "duplicate-index-rehashed",
        lambda rows: rows[1].update(point_index=rows[0]["point_index"]),
    )
    point_edit(
        "fractional-source-row-rehashed",
        lambda rows: rows[0].update(source_row="1.5"),
    )
    point_edit(
        "reversed-physical-rows-rehashed", lambda rows: rows.reverse(), "restore"
    )

    path = make("partial-no-manifest")
    (path / "manifest.dcf").unlink()

    path = make("symlink-member")
    (path / "points.csv").unlink()
    (path / "points.csv").symlink_to(source / "points.csv")

    assert tuple((case["directory"], case["expect"]) for case in cases) == CASE_OUTCOMES
    with manifest_output.open("x", encoding="utf-8") as stream:
        json.dump(cases, stream, indent=2)
        stream.write("\n")
    print(
        json.dumps(
            {
                "corruptions": sum(expect == "reject" for _, expect in CASE_OUTCOMES),
                "reordered_valid_bundles": sum(
                    expect == "restore" for _, expect in CASE_OUTCOMES
                ),
            }
        )
    )


if __name__ == "__main__":
    main()
