#!/bin/bash
# Auto-commit: shell handles diff/IO/git, opencode does text-only analysis (fast single inference)

DIFF_FULL=$(git diff --cached --submodule=diff)
if [ -z "$DIFF_FULL" ]; then
    echo "No staged changes to commit."
    exit 0
fi

# Exclude lock/generated files from LLM analysis (still committed as-is)
DIFF_STAT=$(git diff --cached --stat)
DIFF=$(git diff --cached --submodule=diff -- \
    ':(exclude)*.lock' \
    ':(exclude)package-lock.json' \
    ':(exclude)*.min.js' \
    ':(exclude)*.min.css')

# Truncate diff to avoid exceeding LLM context limit
MAX_CHARS=12000
DIFF_LEN=${#DIFF}
if [ "$DIFF_LEN" -gt "$MAX_CHARS" ]; then
    DIFF="${DIFF:0:$MAX_CHARS}
... [truncated: ${DIFF_LEN} chars total]"
fi

# Show stat and diff
echo "------------------------------------------------------------------"
echo "------------------------- Code Difference ------------------------"
echo "------------------------------------------------------------------"
echo "$DIFF_STAT"
echo ""
echo "$DIFF_FULL"
echo ""

# Single text-only LLM inference with diff pre-embedded (no tool calls -> fast)
OPENCODE_CMD=("opencode_local.sh" "run")
LLM_TIMEOUT="${LLM_TIMEOUT:-180}"
MAX_RETRIES="${MAX_RETRIES:-3}"

PROMPT="Review the staged changes below.
Output EXACTLY 2 lines. No other text. Do not explain your reasoning.

The first line must start with COMMIT: followed by a commit message.
The commit message must begin with one of these prefixes:
Feature: / Fix: / Docs: / Style: / Refactor: / Test:

The second line must start with QUALITY: followed by OK or issues.

Do not echo these instructions or use placeholder text.

stat:
$DIFF_STAT

diff:
$DIFF"

RESULT=""
COMMIT_MSG=""
QUALITY=""

for i in $(seq 1 "$MAX_RETRIES"); do
    echo "Analyzing... (attempt $i/$MAX_RETRIES)"
    RESULT=$(timeout "$LLM_TIMEOUT" "${OPENCODE_CMD[@]}" "$PROMPT" 2>&1)
    EXIT_CODE=$?

    if [ "$EXIT_CODE" -eq 124 ]; then
        echo "[Warning] LLM timed out after ${LLM_TIMEOUT}s"
    elif [ "$EXIT_CODE" -ne 0 ]; then
        echo "[Warning] LLM exited with code $EXIT_CODE"
    fi

    # Robust extraction: allow optional whitespace and markdown code blocks
    # Strip possible markdown fences (some local models wrap output unexpectedly)
    CLEAN_RESULT=$(echo "$RESULT" | sed 's/^[[:space:]]*```[a-zA-Z]*//; s/```[[:space:]]*$//')

    # Find last occurrence even if inline or mid-line
    COMMIT_MSG=$(echo "$CLEAN_RESULT" | grep 'COMMIT:' | tail -1 | sed 's/.*COMMIT:[[:space:]]*//')
    QUALITY=$(echo "$CLEAN_RESULT" | grep 'QUALITY:' | tail -1 | sed 's/.*QUALITY:[[:space:]]*//')

    # Validate that output is not a copy of the instructions
    if [ -n "$COMMIT_MSG" ] && {
        ! echo "$COMMIT_MSG" | grep -qE '^(Feature|Fix|Docs|Style|Refactor|Test): .+' ||
        echo "$COMMIT_MSG" | grep -q '<one-line' ||
        echo "$COMMIT_MSG" | grep -q 'Feature/Fix/Docs/Style/Refactor/Test'
    }; then
        COMMIT_MSG=""
        QUALITY=""
    fi

    # Additional sanity check: commit message must be reasonable length
    if [ -n "$COMMIT_MSG" ]; then
        COMMIT_LEN=${#COMMIT_MSG}
        if [ "$COMMIT_LEN" -lt 5 ] || [ "$COMMIT_LEN" -gt 200 ]; then
            echo "[Warning] Commit message length ($COMMIT_LEN) looks invalid, retrying..."
            COMMIT_MSG=""
            QUALITY=""
        fi
    fi

    if [ -n "$COMMIT_MSG" ] && [ -n "$QUALITY" ]; then
        break
    fi

    if [ "$i" -lt "$MAX_RETRIES" ]; then
        if [ -z "$COMMIT_MSG" ] && [ -z "$QUALITY" ]; then
            echo "[Warning] Failed to parse output, retrying..."
        elif [ -z "$COMMIT_MSG" ]; then
            echo "[Warning] Commit message missing, retrying..."
        else
            echo "[Warning] Quality assessment missing, retrying..."
        fi
        sleep 1
    fi
done

echo "------------------------------------------------------------------"
echo "---------------- Generating commit message by LLM ---------------"
echo "------------------------------------------------------------------"
echo " > $COMMIT_MSG"
echo ""
echo "------------------------------------------------------------------"
echo "---------------------- Quality Check by LLM ----------------------"
echo "------------------------------------------------------------------"
echo " > $QUALITY"
echo ""

# Fallback: if LLM output is unusable, ask user for manual input
if [ -z "$COMMIT_MSG" ]; then
    echo "[Error] Failed to obtain commit message from LLM after $MAX_RETRIES attempts."
    echo "Raw output:"
    echo "$RESULT"
    echo ""
    read -rp "Enter commit message manually (or press Enter to abort): " MANUAL_MSG
    if [ -z "$MANUAL_MSG" ]; then
        echo "コミットを中止しました。"
        exit 0
    fi
    COMMIT_MSG="$MANUAL_MSG"
    QUALITY="MANUAL"
fi

# Skip confirmation if quality is OK; ask only when issues are found
if [ "$QUALITY" != "OK" ]; then
    read -rp "Commit? [y/N]: " CONFIRM
    if [ "$CONFIRM" != "y" ] && [ "$CONFIRM" != "Y" ]; then
        echo "コミットを中止しました。"
        exit 0
    fi
fi

echo "------------------------------------------------------------------"
echo "--------------------------- Git Commit ---------------------------"
echo "------------------------------------------------------------------"
printf '%s\n' "$COMMIT_MSG" | git commit -F -
