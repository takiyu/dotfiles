#!/bin/bash

API_HOST="${API_HOST:-localhost:8001}"
MODEL=$(curl -sf "http://${API_HOST}/v1/models" | jq -r '.data[0].id')

export OPENCODE_PROVIDER_BASE_URL="http://${API_HOST}/v1"
export OPENCODE_PROVIDER_API_KEY="local"
export OPENCODE_MODEL="$MODEL"

exec opencode "$@"
