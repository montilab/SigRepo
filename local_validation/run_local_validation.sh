#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ENV_FILE="${SIGREPO_LOCAL_ENV_FILE:-${SCRIPT_DIR}/.env.local-validation}"

if [[ -f "${ENV_FILE}" ]]; then
  echo "[local-validation] loading environment from ${ENV_FILE}"
  set -a
  # shellcheck disable=SC1090
  source "${ENV_FILE}"
  set +a
fi

echo "[local-validation] running API smoke checks"
"${SCRIPT_DIR}/check_api.sh"

echo "[local-validation] running R validation modules"
Rscript "${SCRIPT_DIR}/run_local_validation.R"
