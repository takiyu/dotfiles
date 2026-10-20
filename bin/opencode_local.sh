#!/bin/bash
set -euo pipefail

echo "Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9001}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"

# Fetch model information; abort if the API server is unreachable
if ! MODELS_JSON=$(curl -sf --connect-timeout 5 --max-time 10 \
        "http://${LLM_API_HOST}/v1/models" | tr -d ' \n'); then
    echo "Error: Failed to communicate with API server at '${LLM_API_HOST}'" >&2
    exit 1
fi
# Parsing model name
if [ -z "${LLM_MODEL:-}" ]; then
    LLM_MODEL=$(echo "$MODELS_JSON" | grep -o '"id":"[^"]*"' \
        | head -1 | cut -d'"' -f4 || true)
fi
# Abort if the response held no usable model id
if [ -z "$LLM_MODEL" ]; then
    echo "Error: No model id parsed from API server at '${LLM_API_HOST}'" >&2
    exit 1
fi
echo " - MODEL: '$LLM_MODEL'"
# Parsing context length
LLM_MAX_CONTEXT="${LLM_MAX_CONTEXT:-$(echo "$MODELS_JSON" \
    | grep -o '"max_model_len":[0-9]*' | cut -d':' -f2 || true)}"
LLM_MAX_OUTPUT="${LLM_MAX_OUTPUT:-32768}"
echo " - LLM_MAX_CONTEXT: $LLM_MAX_CONTEXT"
echo " - LLM_MAX_OUTPUT: $LLM_MAX_OUTPUT"

# Export parameters
export LLM_API_HOST
export LLM_MODEL
export LLM_MAX_CONTEXT
export LLM_MAX_OUTPUT
export OPENCODE_DISABLE_MODELS_FETCH=true

# Use the shared background service (no --standalone). The service freezes
# LLM_* env vars at startup; run 'opencode service restart' to apply changes.
exec opencode "$@"
