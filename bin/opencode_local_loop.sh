#!/bin/bash

if [ $# -eq 0 ]; then
    echo "Usage: $0 <prompt>" >&2
    exit 1
fi

PROMPT="$*"

echo "Starting OpenCode loop with local provider..."
echo " - PROMPT: '$PROMPT'"

while true; do
    opencode_local.sh run "$PROMPT"
done
