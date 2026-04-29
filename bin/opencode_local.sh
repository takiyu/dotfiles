#!/bin/bash

echo "🚀Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9000}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"
MODEL=$(curl -sf "http://${LLM_API_HOST}/v1/models" | tr -d ' \n' | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
echo " - MODEL: '$MODEL'"

export OPENCODE_PROVIDER_BASE_URL="http://${LLM_API_HOST}/v1"
export OPENCODE_PROVIDER_API_KEY="local"
export OPENCODE_MODEL="$MODEL"

exec opencode "$@"
