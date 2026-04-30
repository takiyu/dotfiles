#!/bin/bash

echo "Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9000}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"

if [ -z "$LLM_MODEL" ]; then
    LLM_MODEL=$(curl -sf "http://${LLM_API_HOST}/v1/models" | tr -d ' \n' | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
fi
echo " - MODEL: '$LLM_MODEL'"

export LLM_API_HOST
export LLM_MODEL

export OPENCODE_DISABLE_MODELS_FETCH=true
export OPENCODE_PURE=true

exec opencode "$@"
