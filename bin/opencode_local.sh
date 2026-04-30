#!/bin/bash

echo "Starting OpenCode with local provider..."
LLM_API_HOST="${LLM_API_HOST:-localhost:9000}"
echo " - LLM_API_HOST: '$LLM_API_HOST'"
MODEL=$(curl -sf "http://${LLM_API_HOST}/v1/models" | tr -d ' \n' | grep -o '"id":"[^"]*"' | head -1 | cut -d'"' -f4)
echo " - MODEL: '$MODEL'"

export OPENCODE_CONFIG_CONTENT="{
  \"provider\": {
    \"local\": {
      \"npm\": \"@ai-sdk/openai-compatible\",
      \"name\": \"Local LLM\",
      \"options\": {
        \"baseURL\": \"http://${LLM_API_HOST}/v1\",
        \"apiKey\": \"local\"
      },
      \"models\": {
        \"${MODEL}\": {}
      }
    }
  },
  \"model\": \"local/${MODEL}\",
  \"small_model\": \"local/${MODEL}\"
}"

exec opencode "$@"
