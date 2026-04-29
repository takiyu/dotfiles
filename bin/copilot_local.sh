#!/bin/bash

echo "🚀Starting Copilot with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9000}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"
MODEL=$(curl -sf "http://${LLM_API_HOST}/v1/models" | jq -r '.data[0].id')
echo " - MODEL: '$MODEL'"

export COPILOT_PROVIDER_BASE_URL="http://${LLM_API_HOST}/v1"
export COPILOT_PROVIDER_TYPE="openai"
export COPILOT_PROVIDER_API_KEY="local"
export COPILOT_MODEL="$MODEL"

exec copilot "$@"
