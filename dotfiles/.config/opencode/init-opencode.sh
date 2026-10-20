#!/usr/bin/env bash
# Set up OpenCode and the MCP servers used by this configuration.

set -euo pipefail

CONFIG_DIR="$HOME/.config/opencode"
# Keep these versions in sync with opencode.json.
PLAYWRIGHT_MCP="@playwright/mcp@0.0.83"
BLENDER_MCP_VERSION="2.0.0"


# ------------------------------------------------------------------------------
# ---------------------------------- OpenCode ----------------------------------
# ------------------------------------------------------------------------------
printf '* Install OpenCode\n'
if command -v opencode >/dev/null; then
    printf '  > skip\n'
else
    printf '  > install\n'
    curl -fsSL https://opencode.ai/install | bash
    mkdir -p "$HOME/.local/bin"
    ln -sf "$HOME/.opencode/bin/opencode" "$HOME/.local/bin/opencode"
fi


# ------------------------------------------------------------------------------
# ---------------------------------- Plugins -----------------------------------
# ------------------------------------------------------------------------------
printf '* Install Plugins\n'
if [ -d "$CONFIG_DIR/node_modules/@opencode-ai/plugin" ]; then
    printf '  > skip\n'
else
    printf '  > install\n'
    npm install --prefix "$CONFIG_DIR" --no-audit --no-fund
fi


# ------------------------------------------------------------------------------
# ------------------------------- Playwright MCP -------------------------------
# ------------------------------------------------------------------------------
printf '* Install Playwright MCP %s\n' "$PLAYWRIGHT_MCP"
if npx --offline --yes "$PLAYWRIGHT_MCP" --version >/dev/null 2>&1; then
    printf '  > skip\n'
else
    printf '  > install\n'
    npx --prefer-offline --yes "$PLAYWRIGHT_MCP" --version >/dev/null
fi


# ------------------------------------------------------------------------------
# ----------------------------- Playwright Browser -----------------------------
# ------------------------------------------------------------------------------
printf '* Install Playwright Browser firefox\n'
browser=$(npm exec --prefer-offline --yes --package="$PLAYWRIGHT_MCP" -- \
    playwright install --dry-run firefox 2>/dev/null |
    awk '/Install location/ {print $3; exit}' || true)

if [ -d "$browser" ]; then
    printf '  > skip\n'
else
    printf '  > install\n'
    npm exec --prefer-offline --yes --package="$PLAYWRIGHT_MCP" -- \
        playwright install firefox 2>/dev/null
fi


# ------------------------------------------------------------------------------
# -------------------------------- Blender MCP ---------------------------------
# ------------------------------------------------------------------------------
printf '* Install Blender MCP %s\n' "$BLENDER_MCP_VERSION"
command -v uv >/dev/null || { echo "uv is required" >&2; exit 1; }

if uv tool list | grep -q "blender-mcp v$BLENDER_MCP_VERSION"; then
    printf '  > skip\n'
else
    printf '  > install\n'
    uv tool install --force "blender-mcp==$BLENDER_MCP_VERSION"
fi


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
