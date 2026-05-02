#!/bin/bash
# Send desktop notification when task is complete (session becomes idle)

# Read JSON payload from stdin
INPUT=$(cat)

# Extract cwd for context
CWD=$(echo "${INPUT}" | jq -r '.cwd' 2>/dev/null || echo '')

notify-send 'OpenCode' "Task complete${CWD:+: ${CWD}}"

exit 0
