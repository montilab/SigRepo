#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

mkdir -p results_v4

Rscript test_omicsignature_contract.R
API_EXIT=$?

Rscript test_repository_contract.R
REPO_EXIT=$?

{
  echo "SigRepo reconciled test summary"
  echo "API contract exit code: $API_EXIT"
  echo "Repository contract exit code: $REPO_EXIT"
} > results_v4/all_tests_summary.txt

if [[ "$API_EXIT" -ne 0 || "$REPO_EXIT" -ne 0 ]]; then
  exit 2
fi
