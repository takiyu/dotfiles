#!/bin/bash
# Ensure .task_diary directory exists in the project root (git root) at session end

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd' 2>/dev/null || pwd)

# Resolve git root from cwd; fall back to cwd if not in a git repo
GIT_ROOT=$(git -C "$CWD" rev-parse --show-toplevel 2>/dev/null || echo "$CWD")

DIARY_DIR="$GIT_ROOT/.task_diary"

# Create diary directory if it does not exist
if [ ! -d "$DIARY_DIR" ]; then
    mkdir -p "$DIARY_DIR"
fi

exit 0
