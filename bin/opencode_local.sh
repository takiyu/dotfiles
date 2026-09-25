#!/bin/bash

echo "Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9001}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"

# Fetch model information
MODELS_JSON=$(curl -sf "http://${LLM_API_HOST}/v1/models" | tr -d ' \n')
# Parsing model name
if [ -z "$LLM_MODEL" ]; then
    LLM_MODEL=$(echo "$MODELS_JSON" | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
fi
echo " - MODEL: '$LLM_MODEL'"
# Parsing context length
LLM_MAX_CONTEXT="${LLM_MAX_CONTEXT:-$(echo "$MODELS_JSON" | grep -o '"max_model_len":[0-9]*' | cut -d':' -f2)}"
LLM_MAX_OUTPUT="${LLM_MAX_OUTPUT:-32768}"
echo " - LLM_MAX_CONTEXT: $LLM_MAX_CONTEXT"
echo " - LLM_MAX_OUTPUT: $LLM_MAX_OUTPUT"

# Export parameters
export LLM_API_HOST
export LLM_MODEL
export LLM_MAX_CONTEXT
export LLM_MAX_OUTPUT
export OPENCODE_DISABLE_MODELS_FETCH=true

# Run
exec opencode "$@"
