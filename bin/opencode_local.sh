#!/bin/bash

API_PORT="${API_PORT:-8001}"
MODEL=$(curl -sf "http://localhost:${API_PORT}/v1/models" | jq -r '.data[0].id')

export OPENCODE_PROVIDER_BASE_URL="http://localhost:${API_PORT}/v1"
export OPENCODE_PROVIDER_API_KEY="local"
export OPENCODE_MODEL="$MODEL"

exec opencode "$@"
