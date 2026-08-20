#!/usr/bin/env python3
"""Verify that the SigRepo foundation needed by the skill is present."""

from __future__ import annotations

import argparse
from pathlib import Path
import sys


REQUIRED = [
    Path("codex_reference/README_for_codex.md"),
    Path("codex_reference/helpers/omicsignature_compat.R"),
    Path("codex_reference/references/README_SigRepo_core_conventions.md"),
    Path("codex_reference/references/PROBE_ID_POLICY.md"),
    Path("codex_reference/references/DIFFEXP_POLICY.md"),
    Path("codex_reference/tests/test_omicsignature_contract.R"),
    Path("codex_reference/tests/test_repository_contract.R"),
    Path("codex_reference/approved_examples/LLFS_Sebastiani2024"),
    Path("codex_reference/approved_examples/EMT_Youssef2024"),
    Path("codex_reference/approved_examples/Ding2025"),
]


def find_repo_root(start: Path) -> Path:
    current = start.resolve()
    for candidate in [current, *current.parents]:
        if (candidate / "codex_reference").is_dir():
            return candidate
    raise FileNotFoundError(
        "Could not find a parent directory containing codex_reference."
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--repo-root",
        type=Path,
        help="SigRepo root. Defaults to upward search from the current directory.",
    )
    args = parser.parse_args()

    try:
        root = (
            args.repo_root.resolve()
            if args.repo_root
            else find_repo_root(Path.cwd())
        )
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2

    missing = [path for path in REQUIRED if not (root / path).exists()]
    scenario = root / "codex_tests" / "scenario1_diff_table"

    print(f"Repository root: {root}")
    print(f"Required foundation files present: {not missing}")
    print(f"Visible regression corpus present: {scenario.is_dir()}")

    if missing:
        print("Missing:")
        for path in missing:
            print(f"  - {path}")
        return 2

    tests = root / "codex_reference" / "tests"
    print("\nRun the foundation contracts:")
    if sys.platform.startswith("win"):
        print(f'  "{tests / "run_all_tests_windows_autodetect.cmd"}"')
    else:
        print(f'  bash "{tests / "run_all_tests_linux.sh"}"')

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
