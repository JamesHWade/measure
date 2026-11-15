"""Portable metadata checks for R qualification receipts, using stdlib only."""

import unittest

from verify import PROFILE, runtime_check


def runtime_fixture(path):
    version = "0.0.1.9003"
    r_version = "R version 4.6.2 (2026-10-29)"
    return {
        "profile": PROFILE,
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


class RuntimePaths(unittest.TestCase):
    def test_accepts_absolute_r_library_paths_across_platforms(self):
        for path in (
            "/opt/R/library/measure",
            "/private/tmp/library with spaces/measure",
            "C:/R/library/measure",
            "D:/Users/researcher/R libraries/measure",
            "//server/share/library/measure",
        ):
            with self.subTest(path=path):
                runtime_check(runtime_fixture(path))

    def test_rejects_relative_and_drive_relative_library_paths(self):
        for path in (
            "measure",
            "library/measure",
            "C:library/measure",
            "\\library\\measure",
            "",
        ):
            with self.subTest(path=path), self.assertRaises(ValueError):
                runtime_check(runtime_fixture(path))


if __name__ == "__main__":
    unittest.main()
