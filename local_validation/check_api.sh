#!/usr/bin/env bash

set -euo pipefail

API_HOST="${SIGREPO_LOCAL_API_HOST:-http://127.0.0.1}"
API_PORT="${SIGREPO_LOCAL_API_PORT:-8020}"
BASE_URL="${API_HOST%/}:${API_PORT}"
ENDPOINTS="${SIGREPO_LOCAL_API_ENDPOINTS:-__docs__/}"

FAILURES=0

echo "[api-check] base URL: ${BASE_URL}"

IFS=',' read -r -a endpoint_array <<< "${ENDPOINTS}"

for endpoint in "${endpoint_array[@]}"; do
  endpoint="$(echo "${endpoint}" | xargs)"
  [[ -z "${endpoint}" ]] && continue
  url="${BASE_URL}/$(echo "${endpoint}" | sed 's#^/*##')"

  if curl -fsS "${url}" >/dev/null; then
    echo "[pass] ${url}"
  else
    echo "[fail] ${url}"
    FAILURES=$((FAILURES + 1))
  fi
done

if [[ "${FAILURES}" -gt 0 ]]; then
  echo "[api-check] ${FAILURES} endpoint check(s) failed"
  exit 1
fi

echo "[api-check] all endpoint checks passed"
