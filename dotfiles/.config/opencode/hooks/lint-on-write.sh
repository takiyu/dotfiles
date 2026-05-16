#!/bin/bash
set -euo pipefail


# ------------------------------------------------------------------------------
# -------------------------------- Entry point ---------------------------------
# ------------------------------------------------------------------------------
main() {
    # Main entry: parse JSON input and dispatch file-type checks
    local input tool_name cwd file_path
    local violations=''

    # Read stdin as JSON input from the caller
    input=$(cat)
    tool_name=$(extract_tool_name "$input")

    if [ "$tool_name" != 'edit' ] && [ "$tool_name" != 'create' ]; then
        exit 0
    fi

    cwd=$(extract_cwd "$input")
    file_path=$(extract_file_path "$input")

    if [ -z "$file_path" ]; then
        exit 0
    fi

    file_path=$(resolve_file_path "$cwd" "$file_path")

    if [ ! -f "$file_path" ]; then
        exit 0
    fi

    # Run Python checks when the file has a .py extension
    if echo "$file_path" | grep -qE '\.py$'; then
        violations=$(append_violations "$violations" \
            "$(check_python "$file_path")")
    fi

    # Run TypeScript checks when the file has a .ts or .tsx extension
    if echo "$file_path" | grep -qE '\.(ts|tsx)$'; then
        violations=$(append_violations "$violations" \
            "$(check_typescript "$file_path")")
    fi

    # Run JavaScript checks when the file has a .js or .jsx extension
    if echo "$file_path" | grep -qE '\.(js|jsx)$'; then
        violations=$(append_violations "$violations" \
            "$(check_javascript "$file_path")")
    fi

    # Run shell checks when the file has a .sh extension
    if echo "$file_path" | grep -qE '\.sh$'; then
        violations=$(append_violations "$violations" \
            "$(check_shell "$file_path")")
    fi

    output_violations "$file_path" "$violations"

    exit 0
}


# ------------------------------------------------------------------------------
# ---------------------------- Violation formatting ----------------------------
# ------------------------------------------------------------------------------
output_violations() {
    # Print collected violations framed with a header to stderr
    local file_path="$1"
    local violations="$2"
    if [ -n "$violations" ]; then
        echo '' >&2
        echo "╔══ ⚠  lint-on-write: $(basename "$file_path") ══" >&2
        echo "$violations" | grep -v '^$' >&2
        echo '╚═══════════════════════════════════════════════' >&2
        echo '' >&2
    fi
}


# ------------------------------------------------------------------------------
# ---------------------------------- Helpers -----------------------------------
# ------------------------------------------------------------------------------
extract_tool_name() {
    # Extract toolName field from JSON input via jq
    echo "$1" | jq -r '.toolName' 2>/dev/null || echo ''
}


extract_cwd() {
    # Extract cwd field from JSON input via jq, falling back to pwd
    echo "$1" | jq -r '.cwd' 2>/dev/null || pwd
}


extract_file_path() {
    # Extract nested path from toolArgs JSON string via jq
    local args_str
    args_str=$(echo "$1" | jq -r '.toolArgs' 2>/dev/null || echo '{}')
    echo "$args_str" | jq -r '.path // empty' 2>/dev/null
}


resolve_file_path() {
    # Resolve a relative path against cwd into an absolute path
    local cwd="$1"
    local file_path="$2"
    if [[ "$file_path" != /* ]]; then
        file_path="$cwd/$file_path"
    fi
    echo "$file_path"
}


append_violations() {
    # Concatenate two violation strings, avoiding extra blank lines
    local base="$1"
    local addition="$2"
    if [ -z "$addition" ]; then
        echo "$base"
        return
    fi
    if [ -z "$base" ]; then
        echo "$addition"
        return
    fi
    printf '%s\n%s\n' "$base" "$addition"
}


format_violation() {
    # Prefix each grep result with file path and suffix with severity/message
    local file_path="$1"
    local severity="$2"
    local message="$3"
    while IFS= read -r line; do
        printf '%s:%s -> [%s] %s\n' \
            "$file_path" "$line" "$severity" "$message"
    done
}


# ------------------------------------------------------------------------------
# ------------------------------- Python checks --------------------------------
# ------------------------------------------------------------------------------
check_python() {
    # Run all Python-specific lint checks and return combined violations
    local file_path="$1"
    local result=''

    result=$(append_violations "$result" \
        "$(check_python_pep8 "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_docstrings "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_typing "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_none_union "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_empty_literals "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_def_parens "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_call_parens "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_closing_paren_arrow "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_multiline_ternary "$file_path")")
    result=$(append_violations "$result" \
        "$(check_python_section_delimiters "$file_path")")

    echo "$result"
}


check_python_section_delimiters() {
    # Run section delimiter checks for Python files (79 chars, # prefix)
    _check_file_section_delimiters "$1" 79 '# '
}


check_python_pep8() {
    # Run pycodestyle for PEP8 compliance (max line length 79)
    local file_path="$1"
    local pcs
    if ! command -v pycodestyle >/dev/null 2>&1; then
        return
    fi
    pcs=$(pycodestyle --max-line-length=79 "$file_path" 2>/dev/null || true)
    if [ -n "$pcs" ]; then
        echo "$pcs"
    fi
}


check_python_docstrings() {
    # Detect forbidden docstrings (triple-quoted strings)
    local file_path="$1"
    local match
    match=$(grep -nE '^\s*("""|\x27\x27\x27)' "$file_path" 2>/dev/null \
        || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Docstring forbidden'
    fi
}


check_python_typing() {
    # Flag deprecated typing module usage (Dict, List, Tuple, Union, Any)
    local file_path="$1"
    local match
    match=$(grep -nE \
        '(typing\.(Dict|List|Tuple|Union|Any)\b|from typing import[^#]*(Dict|List|Tuple|Union|Any))' \
        "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Use dict[]/list[]/tuple[]/Optional[X], not typing.*'
    fi
}


check_python_none_union() {
    # Flag union-with-None syntax requiring Optional[X] instead
    local file_path="$1"
    local match
    match=$(grep -nE '\w+\s*\|\s*None\b|None\s*\|\s*\w+' "$file_path" \
        2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Use Optional[X], not X | None'
    fi
}


check_python_empty_literals() {
    # Flag empty dict {} and empty list [] literals (use dict()/list())
    local file_path="$1"
    local match_dict match_list

    match_dict=$(grep -nE '=\s*\{\}\s*(#|$)' "$file_path" 2>/dev/null \
        || true)
    if [ -n "$match_dict" ]; then
        echo "$match_dict" | format_violation "$file_path" '高' \
            'Use dict() for empty dict, not {}'
    fi

    match_list=$(grep -nE '=\s*\[\]\s*(#|$)' "$file_path" 2>/dev/null \
        || true)
    if [ -n "$match_list" ]; then
        echo "$match_list" | format_violation "$file_path" '高' \
            'Use list() for empty list, not []'
    fi
}


check_python_def_parens() {
    # Flag function definitions with ( on its own line
    local file_path="$1"
    local match
    match=$(grep -nP '^\s*(async\s+)?def\s+\w+\($' "$file_path" \
        2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'First arg must be on same line as def funcname('
    fi
}


check_python_call_parens() {
    # Flag function calls with ( alone at end of line (hanging indent) via Python parser
    local file_path="$1"
    local match
    if ! command -v python3 >/dev/null 2>&1; then
        return
    fi
    match=$(PY_FILE="$file_path" python3 -c '
import os
path = os.environ["PY_FILE"]
lines = open(path).readlines()
for i, line in enumerate(lines[:-1]):
    s = line.rstrip()
    if not s.endswith("(") or len(s) < 2:
        continue
    if not (s[-2].isalnum() or s[-2] == "_"):
        continue
    if s.lstrip().startswith(("def ", "async def ")):
        continue
    paren_col = len(s) - 1
    nxt = lines[i + 1]
    nxt_stripped = nxt.lstrip()
    if not nxt_stripped.strip():
        continue
    if len(nxt) - len(nxt_stripped) != paren_col + 1:
        print(f"{i+1}:{s}")
' 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'First arg must be on same line as (; align continuation to opening ('
    fi
}


check_python_closing_paren_arrow() {
    # Flag closing ) at column 0 before -> return annotation
    local file_path="$1"
    local match
    match=$(grep -nP '^\)\s*->' "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'Closing ) at col 0; must align to opening ( column'
    fi
}


check_python_multiline_ternary() {
    # Flag multi-line ternary expressions starting with if/else
    local file_path="$1"
    local match
    match=$(grep -nP '^\s+if\s.+\belse\b' "$file_path" 2>/dev/null \
        || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'Multi-line ternary; rewrite as if/else block'
    fi
}


# ------------------------------------------------------------------------------
# ----------------------------- TypeScript checks ------------------------------
# ------------------------------------------------------------------------------
check_typescript() {
    # Run all TypeScript-specific lint checks and return combined violations
    local file_path="$1"
    local result=''

    result=$(append_violations "$result" \
        "$(check_typescript_class_component "$file_path")")
    result=$(append_violations "$result" \
        "$(check_typescript_any_type "$file_path")")
    result=$(append_violations "$result" \
        "$(check_typescript_interface_props "$file_path")")
    result=$(append_violations "$result" \
        "$(check_typescript_untyped_params "$file_path")")
    result=$(append_violations "$result" \
        "$(check_typescript_section_delimiters "$file_path")")

    echo "$result"
}


check_typescript_section_delimiters() {
    # Check section delimiter lines for TypeScript files (80 chars, // prefix)
    _check_file_section_delimiters "$1" 80 '// '
}


check_javascript_section_delimiters() {
    # Check section delimiter lines for JavaScript files (80 chars, // prefix)
    _check_file_section_delimiters "$1" 80 '// '
}


check_typescript_class_component() {
    # Flag class-based React components (use function components instead)
    local file_path="$1"
    local match
    match=$(grep -nE 'extends\s+(React\.)?(Component|PureComponent)\b' \
        "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Use function component, not class component'
    fi
}


check_typescript_any_type() {
    # Flag variables/parameters annotated with any type
    local file_path="$1"
    local match
    match=$(grep -nE ':\s*any\b|<any>' "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            "Avoid 'any' type; use explicit type"
    fi
}


check_typescript_interface_props() {
    # Flag interface usage for Props/State (use type instead)
    local file_path="$1"
    local match
    match=$(grep -nE '^(export\s+)?interface\s+\w+(Props|State)\b' \
        "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            "Use 'type' instead of 'interface' for Props/State"
    fi
}


check_typescript_untyped_params() {
    # Flag function parameters missing type annotations
    local file_path="$1"
    local match
    match=$(grep -nE 'function\s+\w+\s*\([^)]*[^):]\s*[^):]\)' \
        "$file_path" 2>/dev/null | grep -v '//' || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'Type all function parameters'
    fi
}


# ------------------------------------------------------------------------------
# ----------------------------- JavaScript checks ------------------------------
# ------------------------------------------------------------------------------
check_javascript() {
    # Run all JavaScript-specific lint checks and return combined violations
    local file_path="$1"
    local result=''

    result=$(append_violations "$result" \
        "$(check_javascript_class_component "$file_path")")
    result=$(append_violations "$result" \
        "$(check_javascript_mixed_modules "$file_path")")
    result=$(append_violations "$result" \
        "$(check_javascript_pascalcase_function "$file_path")")
    result=$(append_violations "$result" \
        "$(check_javascript_console_logs "$file_path")")
    result=$(append_violations "$result" \
        "$(check_javascript_module_level_let "$file_path")")
    result=$(append_violations "$result" \
        "$(check_javascript_section_delimiters "$file_path")")

    echo "$result"
}


check_javascript_class_component() {
    # Flag class-based React components (use function components instead)
    local file_path="$1"
    local match
    match=$(grep -nE 'extends\s+(React\.)?(Component|PureComponent)\b' \
        "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Use function component, not class component'
    fi
}


check_javascript_mixed_modules() {
    # Flag mixed require() and import/export in the same file
    local file_path="$1"
    local has_require has_import
    has_require=$(grep -cE '\brequire\s*\(' "$file_path" 2>/dev/null \
        || echo 0)
    has_import=$(grep -cE '\bimport\s+' "$file_path" 2>/dev/null \
        || echo 0)
    has_require=$(echo "$has_require" | tr -d '[:space:]')
    has_import=$(echo "$has_import" | tr -d '[:space:]')
    if [ "$has_require" -gt 0 ] && [ "$has_import" -gt 0 ]; then
        printf '%s:0: -> [高] Mixed module systems: do not mix require() and import/export\n' \
            "$file_path"
    fi
}


check_javascript_pascalcase_function() {
    # Flag non-component functions using PascalCase or missing use- prefix
    local file_path="$1"
    local match
    match=$(grep -nP \
        '^(export\s+)?(async\s+)?function\s+(?!use)[A-Z][a-zA-Z0-9_]*\s*\([^)]*\)\s*\{' \
        "$file_path" 2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'Non-component function with PascalCase; use camelCase or add use prefix for hooks'
    fi
}


check_javascript_console_logs() {
    # Flag leftover console logging calls in production code
    local file_path="$1"
    local match
    match=$(grep -nE '\bconsole\.(log|warn|error|info)\s*\(' "$file_path" \
        2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '低' \
            'Remove console.log/debug statements from production code'
    fi
}


check_javascript_module_level_let() {
    # Flag mutable module-level declarations using let or var
    local file_path="$1"
    local match
    match=$(grep -nP '^(export\s+)?(let|var)\s+\w+' "$file_path" \
        2>/dev/null || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            'Avoid mutable module-level state (let/var at top level)'
    fi
}


# ------------------------------------------------------------------------------
# -------------------------------- Shell checks --------------------------------
# ------------------------------------------------------------------------------
check_shell() {
    # Run all shell-specific lint checks and return combined violations
    local file_path="$1"
    local result=''

    result=$(append_violations "$result" \
        "$(check_shell_shebang "$file_path")")
    result=$(append_violations "$result" \
        "$(check_shell_eval "$file_path")")
    result=$(append_violations "$result" \
        "$(check_shell_strict_mode "$file_path")")
    result=$(append_violations "$result" \
        "$(check_shell_cd_error "$file_path")")
    result=$(append_violations "$result" \
        "$(check_shell_section_delimiters "$file_path")")

    echo "$result"
}


check_shell_section_delimiters() {
    # Check section delimiter lines: must be exactly 80 chars, no trailing spaces
    _check_file_section_delimiters "$1" 80 '# '
}


check_shell_shebang() {
    # Verify the file starts with a valid bash or sh shebang
    local file_path="$1"
    local head
    head=$(head -n1 "$file_path" 2>/dev/null || true)
    if ! echo "$head" | grep -qE '^#!/bin/(bash|sh)'; then
        printf '%s:1: -> [高] Missing shebang (#!/bin/bash or #!/bin/sh) at top of file\n' \
            "$file_path"
    fi
}


check_shell_eval() {
    # Detect dangerous eval builtin usage (skip lines that are purely comments)
    local file_path="$1"
    local match
    match=$(grep -nE '\beval\s' "$file_path" 2>/dev/null \
        | grep -vE '^\s*#' || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '高' \
            'Avoid eval; use indirect expansion or arrays instead'
    fi
}


check_shell_strict_mode() {
    # Ensure bash scripts use set -euo pipefail and sh scripts use set -eu
    local file_path="$1"
    local head
    head=$(head -n1 "$file_path" 2>/dev/null || true)
    if echo "$head" | grep -qF '#!/bin/bash'; then
        if ! grep -qE '^set\s+-euo?\s+pipefail' "$file_path" 2>/dev/null; then
            printf '%s:0: -> [中] Missing set -euo pipefail at start of script\n' \
                "$file_path"
        fi
    elif echo "$head" | grep -qF '#!/bin/sh'; then
        if ! grep -qE '^set\s+-eu' "$file_path" 2>/dev/null; then
            printf '%s:0: -> [中] Missing set -eu at start of script\n' \
                "$file_path"
        fi
    fi
}


check_shell_cd_error() {
    # Detect cd commands lacking error handling (|| exit / || return)
    local file_path="$1"
    local match
    match=$(grep -nE '^\s*cd\s+\S+' "$file_path" 2>/dev/null \
        | grep -v '||' | grep -v '&&' || true)
    if [ -n "$match" ]; then
        echo "$match" | format_violation "$file_path" '中' \
            "cd without error check; use 'cd dir || exit 1'"
    fi
}


_check_file_section_delimiters() {
    # Shared section delimiter check for any file type.
    # Arguments: file_path, expected_length, comment_prefix
    local file_path="$1"
    local expected_length="$2"
    local comment_prefix="$3"
    local line_num=0
    local line
    local len

    # Read all lines into an array to allow lookahead/lookbehind
    local -a lines=()
    while IFS= read -r line || [ -n "$line" ]; do
        lines+=("$line")
    done < "$file_path"
    local total_lines=${#lines[@]}

    local prefix_pattern
    prefix_pattern=$(printf '%s' "$comment_prefix" | sed 's/\//\\\//g')

    for ((line_num = 0; line_num < total_lines; line_num++)); do
        line="${lines[$line_num]}"
        len=$(printf '%s' "$line" | wc -c)

        # Skip non-comment lines
        if [[ "$line" != "$comment_prefix"* ]]; then
            continue
        fi

        # Extract the content after the comment prefix
        local content
        content=$(printf '%s' "$line" | sed "s/^$prefix_pattern//")
        local content_len
        content_len=$(printf '%s' "$content" | wc -c)

        if printf '%s' "$content" | grep -qE '^-+'; then
            # This line starts with dashes: check delimiter/separator rules
            if [ "$len" -ne "$expected_length" ]; then
                printf '%s:%s: -> [\u9ad8] Section delimiter must be exactly %s chars (got %s)\n' \
                    "$file_path" "$((line_num + 1))" "$expected_length" "$len"
            fi
            # Check for trailing spaces
            if printf '%s' "$line" | grep -qE ' $'; then
                printf '%s:%s: -> [\u9ad8] Section delimiter has trailing space\n' \
                    "$file_path" "$((line_num + 1))"
            fi
            # Check named section header format (line 2 of a 3-line block)
            # We skip pure separator lines (no text between hyphens)
            if printf '%s' "$content" | grep -qE '^--+ [^ -].*[^ -] --+$'; then
                # Extract the name part (between hyphens)
                local left_dashes
                local right_dashes
                left_dashes=$(printf '%s' "$content" | sed 's/\(-*\) .*/\1/')
                right_dashes=$(printf '%s' "$content" | sed 's/.* \(-*\)$/\1/')
                local left_len
                local right_len
                left_len=$(printf '%s' "$left_dashes" | wc -c)
                right_len=$(printf '%s' "$right_dashes" | wc -c)
                local diff=$((left_len - right_len))
                if [ "$diff" -lt 0 ]; then
                    diff=$((-diff))
                fi
                if [ "$diff" -gt 1 ]; then
                    printf '%s:%s: -> [\u9ad8] Section header hyphens unbalanced: left=%s, right=%s\n' \
                        "$file_path" "$((line_num + 1))" "$left_len" "$right_len"
                fi
                # Right side should be >= left side when odd difference
                if [ "$right_len" -lt "$left_len" ]; then
                    printf '%s:%s: -> [\u9ad8] Section header: right side should have more hyphens than left\n' \
                        "$file_path" "$((line_num + 1))"
                fi
            fi
        else
            # Content does NOT start with dashes.
            # Only flag as a broken section header when sandwiched between
            # two separator lines (comment lines content is all dashes).
            if [ "$content_len" -gt 0 ]; then
                local prev_idx=$((line_num - 1))
                local next_idx=$((line_num + 1))
                local prev_content
                local next_content
                local is_flanked=0

                if [ "$prev_idx" -ge 0 ] \
                    && [ "$next_idx" -lt "$total_lines" ]; then
                    prev_content=$(printf '%s' "${lines[$prev_idx]}" \
                        | sed "s/^$prefix_pattern//")
                    next_content=$(printf '%s' "${lines[$next_idx]}" \
                        | sed "s/^$prefix_pattern//")
                    if printf '%s' "$prev_content" | grep -qE '^--+$' \
                        && printf '%s' "$next_content" | grep -qE '^--+$'; then
                        is_flanked=1
                    fi
                fi

                if [ "$is_flanked" -eq 1 ]; then
                    printf '%s:%s: -> [\u9ad8] Missing section delimiter dashes (expected %s chars)\n' \
                        "$file_path" "$((line_num + 1))" "$expected_length"
                fi
            fi
        fi
    done
}


main "$@"


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------