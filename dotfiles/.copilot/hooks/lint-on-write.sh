#!/bin/bash
# Lint check auto-triggered after each file write (edit/create tool use)

INPUT=$(cat)

# Extract tool name
TOOL_NAME=$(echo "$INPUT" | jq -r '.toolName' 2>/dev/null || echo '')

# Only act on file write operations
if [ "$TOOL_NAME" != 'edit' ] && [ "$TOOL_NAME" != 'create' ]; then
    exit 0
fi

# Extract cwd and file path from toolArgs (toolArgs is a nested JSON string)
CWD=$(echo "$INPUT" | jq -r '.cwd' 2>/dev/null || pwd)
ARGS_STR=$(echo "$INPUT" | jq -r '.toolArgs' 2>/dev/null || echo '{}')
FILE_PATH=$(echo "$ARGS_STR" | jq -r '.path // empty' 2>/dev/null)

if [ -z "$FILE_PATH" ]; then
    exit 0
fi

# Resolve to absolute path
if [[ "$FILE_PATH" != /* ]]; then
    FILE_PATH="$CWD/$FILE_PATH"
fi

if [ ! -f "$FILE_PATH" ]; then
    exit 0
fi

VIOLATIONS=''

# -----------------------------------------------------------------------------
# Python checks
# -----------------------------------------------------------------------------
if echo "$FILE_PATH" | grep -qE '\.py$'; then

    # PEP8 style (line length=79, indentation, whitespace)
    if command -v pycodestyle >/dev/null 2>&1; then
        PCS=$(pycodestyle --max-line-length=79 "$FILE_PATH" 2>&1 || true)
        if [ -n "$PCS" ]; then
            VIOLATIONS="$VIOLATIONS
$PCS"
        fi
    fi

    # Docstrings (forbidden per coding rules)
    MATCH=$(grep -nE '^\s*("""|\x27\x27\x27)' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Docstring forbidden"
    fi

    # typing.Dict / List / Tuple / Union / Any or "from typing import ..." (use built-in generics)
    MATCH=$(grep -nE '(typing\.(Dict|List|Tuple|Union|Any)\b|from typing import[^#]*(Dict|List|Tuple|Union|Any))' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use dict[]/list[]/tuple[]/Optional[X], not typing.*"
    fi

    # X | None / None | X syntax (use Optional[X])
    MATCH=$(grep -nE '\w+\s*\|\s*None\b|None\s*\|\s*\w+' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use Optional[X], not X | None"
    fi

    # {} empty dict literal (use dict())
    MATCH=$(grep -nE '=\s*\{\}\s*(#|$)' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use dict() for empty dict, not {}"
    fi

    # [] empty list literal (use list())
    MATCH=$(grep -nE '=\s*\[\]\s*(#|$)' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use list() for empty list, not []"
    fi
fi

# -----------------------------------------------------------------------------
# TypeScript / TSX checks
# -----------------------------------------------------------------------------
if echo "$FILE_PATH" | grep -qE '\.(ts|tsx)$'; then

    # Class-based React component (use function component)
    MATCH=$(grep -nE 'extends\s+(React\.)?(Component|PureComponent)\b' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use function component, not class component"
    fi

    # any type (forbidden)
    MATCH=$(grep -nE ':\s*any\b|<any>' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Avoid 'any' type; use explicit type"
    fi

    # interface used for props/state (use type instead)
    MATCH=$(grep -nE '^(export\s+)?interface\s+\w+(Props|State)\b' "$FILE_PATH" 2>/dev/null || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [高] Use 'type' instead of 'interface' for Props/State"
    fi

    # Untyped function parameters (e.g. function foo(x) without type annotation)
    MATCH=$(grep -nE 'function\s+\w+\s*\([^)]*[^):]\s*[^):]\)' "$FILE_PATH" 2>/dev/null \
        | grep -v '//' || true)
    if [ -n "$MATCH" ]; then
        VIOLATIONS="$VIOLATIONS
$(echo "$MATCH" | sed "s|^|$FILE_PATH:|") → [中] Type all function parameters"
    fi
fi

# -----------------------------------------------------------------------------
# Output violations
# -----------------------------------------------------------------------------
if [ -n "$VIOLATIONS" ]; then
    echo '' >&2
    echo "╔══ ⚠  lint-on-write: $(basename "$FILE_PATH") ══" >&2
    echo "$VIOLATIONS" | grep -v '^$' >&2
    echo '╚═══════════════════════════════════════════════' >&2
    echo '' >&2
fi

exit 0
