#!/bin/bash
# Auto-commit: shell handles diff/IO/git, copilot does text-only analysis (fast single inference)

DIFF=$(git diff --cached --submodule=diff)
if [ -z "$DIFF" ]; then
    echo "No staged changes to commit."
    exit 0
fi

# Show diff
echo "------------------------------------------------------------------"
echo "------------------------- Code Difference ------------------------"
echo "------------------------------------------------------------------"
echo "$DIFF"
echo ""

# Single text-only LLM inference with diff pre-embedded (no tool calls -> fast)
echo "Analyzing..."
RESULT=$(copilot --allow-all --model gpt-5-mini -p \
"Output EXACTLY two lines and nothing else:
COMMIT: Feature/Fix/Docs/Style/Refactor/Test: <one-line description>
QUALITY: OK (or issues in Japanese with [高]/[中]/[低] labels, use / as separator)

diff:
$DIFF" 2>&1)

# Extract commit message and quality result
COMMIT_MSG=$(echo "$RESULT" | grep "^COMMIT:" | tail -1 | sed 's/^COMMIT: //')
QUALITY=$(echo "$RESULT" | grep "^QUALITY:" | tail -1 | sed 's/^QUALITY: //')

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

if [ -z "$COMMIT_MSG" ]; then
    echo "[Error] Failed to extract commit message. Raw output:"
    echo "$RESULT"
    exit 1
fi

# Skip confirmation if quality is OK; ask only when issues are found
if [ "$QUALITY" != "OK" ]; then
    read -p "Commit? [y/N]: " CONFIRM
    if [ "$CONFIRM" != "y" ] && [ "$CONFIRM" != "Y" ]; then
        echo "コミットを中止しました。"
        exit 0
    fi
fi

echo "------------------------------------------------------------------"
echo "--------------------------- Git Commit ---------------------------"
echo "------------------------------------------------------------------"
git commit -m "$COMMIT_MSG"
