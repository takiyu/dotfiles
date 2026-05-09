#!/usr/bin/env bash
# Desktop notification triggered by opencode permission-ask hook.
# Reads JSON from stdin, extracts permission details, and sends a notification.

set -euo pipefail

# Read JSON payload from stdin
PAYLOAD=$(cat)

# Extract fields using basic grep/sed (no jq dependency)
PERMISSION_TYPE=$(echo "$PAYLOAD" | grep -o '"permissionType": *"[^"]*"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
TITLE=$(echo "$PAYLOAD" | grep -o '"title": *"[^"]*"' | head -1 | sed 's/.*: *"\([^"]*\)".*/\1/')
PATTERNS=$(echo "$PAYLOAD" | grep -o '"patterns": *\[[^]]*\]' | head -1 | sed 's/.*\[\(.*\)\].*/\1/' | tr ',' ' ' | tr -d '"')

# Fallbacks
PERMISSION_TYPE="${PERMISSION_TYPE:-unknown}"
TITLE="${TITLE:-Permission Requested}"

MSG="Type: ${PERMISSION_TYPE}"
if [ -n "$PATTERNS" ]; then
    MSG="${MSG} | Patterns: ${PATTERNS}"
fi

# Send notification
notify-send -u critical "OpenCode: ${TITLE}" "$MSG"
