#!/bin/bash
set -euo pipefail

echo "Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9001}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"

# Fetch model information
MODELS_JSON=$(curl -sf "http://${LLM_API_HOST}/v1/models" | tr -d ' \n' || true)
# Parsing model name
if [ -z "${LLM_MODEL:-}" ]; then
    LLM_MODEL=$(echo "$MODELS_JSON" | grep -o '"id":"[^"]*"' \
        | head -1 | cut -d'"' -f4 || true)
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

# Run opencode bound to this process with --standalone (v1-like), so all work
# stops on exit instead of persisting in the shared background service.

# Server/management subcommands keep the shared background service behavior
passthrough_cmds="acp api auth debug models mcp pair plugin reload serve"
passthrough_cmds="$passthrough_cmds service session stats"
passthrough_cmds="$passthrough_cmds uninstall update upgrade"

# run/mini take the flag after the name, the bare TUI takes it up front
if [ "${1:-}" = "run" ] || [ "${1:-}" = "mini" ]; then
    exec opencode "$1" --standalone "${@:2}"
fi
case " $passthrough_cmds " in
    *" ${1:-} "*) exec opencode "$@" ;;
    *)        exec opencode --standalone "$@" ;;
esac
