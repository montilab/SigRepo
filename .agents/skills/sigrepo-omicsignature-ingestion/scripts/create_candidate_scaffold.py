#!/usr/bin/env python3
"""Create a non-overwriting development-example scaffold after accepted preflight."""

from __future__ import annotations

import argparse
from pathlib import Path
import shutil
import sys


PATTERN_TO_ASSET = {
    "membership": "membership_selected_result_template.Rmd",
    "selected-result": "membership_selected_result_template.Rmd",
    "differential": "differential_table_template.Rmd",
}


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
    parser.add_argument("--study-key", required=True)
    parser.add_argument(
        "--pattern",
        required=True,
        choices=sorted(PATTERN_TO_ASSET),
    )
    parser.add_argument("--repo-root", type=Path)
    args = parser.parse_args()

    root = (
        args.repo_root.resolve()
        if args.repo_root
        else find_repo_root(Path.cwd())
    )

    skill_root = Path(__file__).resolve().parents[1]
    assets = skill_root / "assets"
    target = (
        root
        / "codex_reference"
        / "development_examples"
        / args.study_key
    )

    if target.exists():
        print(f"ERROR: Refusing to overwrite existing directory: {target}", file=sys.stderr)
        return 2

    target.mkdir(parents=True)
    (target / "reference_data").mkdir()

    source_rmd = assets / PATTERN_TO_ASSET[args.pattern]
    target_rmd = target / f"{args.study_key}_development.Rmd"
    shutil.copy2(source_rmd, target_rmd)

    readme_text = (assets / "DEVELOPMENT_README_TEMPLATE.md").read_text(
        encoding="utf-8"
    )
    readme_text = readme_text.replace("{{STUDY_KEY}}", args.study_key)
    (target / "README.md").write_text(readme_text, encoding="utf-8")

    print(f"Created: {target}")
    print(f"Template: {target_rmd}")
    print("Next: replace placeholders only after the preflight decisions are accepted.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
