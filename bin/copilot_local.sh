#!/bin/bash

API_HOST="${API_HOST:-localhost:8001}"
MODEL=$(curl -sf "http://${API_HOST}/v1/models" | jq -r '.data[0].id')

export COPILOT_PROVIDER_BASE_URL="http://${API_HOST}/v1"
export COPILOT_PROVIDER_TYPE="openai"
export COPILOT_PROVIDER_API_KEY="local"
export COPILOT_MODEL="$MODEL"

exec copilot "$@"
