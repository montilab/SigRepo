#!/usr/bin/env bash

set -euo pipefail

MCP_HOST="${SIGREPO_LOCAL_MCP_HOST:-}"
MCP_PORT="${SIGREPO_LOCAL_MCP_PORT:-}"

if [[ -z "${MCP_HOST}" || -z "${MCP_PORT}" ]]; then
  echo "[mcp-check] SIGREPO_LOCAL_MCP_HOST/SIGREPO_LOCAL_MCP_PORT not configured, skipping"
  exit 0
fi

BASE_URL="${MCP_HOST%/}:${MCP_PORT}"

echo "[mcp-check] base URL: ${BASE_URL}"

response=$(curl -fsS -X POST "${BASE_URL}" \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}')

FAILURES=0

for tool in list_vocabulary search_signatures get_signature_context compare_signatures; do
  if echo "${response}" | grep -q "\"${tool}\""; then
    echo "[pass] tools/list includes ${tool}"
  else
    echo "[fail] tools/list is missing ${tool}"
    FAILURES=$((FAILURES + 1))
  fi
done

if [[ "${FAILURES}" -gt 0 ]]; then
  echo "[mcp-check] ${FAILURES} tool check(s) failed"
  exit 1
fi

echo "[mcp-check] all tool checks passed"
