#!/usr/bin/env bash
# Send desktop notification when task is complete (session becomes idle)

set -euo pipefail

# Read JSON payload from stdin
INPUT=$(cat)

# Extract cwd with jq if available, fallback to basic sed/grep
if command -v jq >/dev/null 2>&1; then
    CWD=$(echo "${INPUT}" | jq -r '.cwd // empty')
else
    CWD=$(echo "${INPUT}" | grep -o '"cwd": *"[^"]*"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/' || true)
fi

notify-send 'OpenCode' "Task complete${CWD:+: ${CWD}}"

exit 0
