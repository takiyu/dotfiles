#!/bin/bash
set -euo pipefail


# ------------------------------------------------------------------------------
# -------------------------------- Entry point ---------------------------------
# ------------------------------------------------------------------------------
main() {
    local input tool_name cwd file_path violations
    input=$(cat)
    tool_name=$(jq -r '.toolName' <<<"$input" 2>/dev/null || echo '')

    if [[ "$tool_name" != 'edit' && "$tool_name" != 'write' ]]; then
        exit 0
    fi

    cwd=$(jq -r '.cwd // empty' <<<"$input" 2>/dev/null || pwd)
    file_path=$(jq -r '.toolArgs | fromjson? | .path // .filePath // empty' \
        <<<"$input" 2>/dev/null || echo '')

    if [[ -z "$file_path" ]]; then
        exit 0
    fi

    if [[ "$file_path" != /* ]]; then
        file_path="$cwd/$file_path"
    fi

    if [[ ! -f "$file_path" ]]; then
        exit 0
    fi

    violations=''
    case "$file_path" in
        *.py)  violations=$(check_python "$file_path") ;;
        *.ts|*.tsx) violations=$(check_typescript "$file_path") ;;
        *.js|*.jsx) violations=$(check_javascript "$file_path") ;;
        *.sh)  violations=$(check_shell "$file_path") ;;
    esac

    output_violations "$file_path" "$violations"
    exit 0
}


# ------------------------------------------------------------------------------
# --------------------------- Violation formatting -----------------------------
# ------------------------------------------------------------------------------
output_violations() {
    local file_path="$1" violations="$2"
    if [[ -n "$violations" ]]; then
        {
            echo ''
            echo "╔══ ⚠  lint-on-write: $(basename "$file_path") ══"
            grep -v '^$' <<<"$violations"
            echo '╚═══════════════════════════════════════════════'
            echo ''
        } >&2
    fi
}

format_violation() {
    local file_path="$1" severity="$2" message="$3"
    while IFS= read -r line; do
        printf '%s:%s -> [%s] %s\n' "$file_path" "$line" "$severity" "$message"
    done
}


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------ Implementation --------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------- Python checks --------------------------------
# ------------------------------------------------------------------------------
check_python() {
    local file_path="$1" v=''

    # Performance: run grep-based checks in parallel subshells where safe,
    # then concatenate results.
    local docstring_match empty_dict_match empty_list_match
    docstring_match=$(grep -nE '^\s*("""|\x27\x27\x27)' "$file_path" \
        2>/dev/null || true)
    empty_dict_match=$(grep -nE '=\s*\{\}\s*(#|$)' "$file_path" \
        2>/dev/null || true)
    empty_list_match=$(grep -nE '=\s*\[\]\s*(#|$)' "$file_path" \
        2>/dev/null || true)

    if [[ -n "$docstring_match" ]]; then
        v=$(printf '%s\n' "$v" \
            "$(format_violation "$file_path" '高' \
                'Docstring forbidden' <<<"$docstring_match")")
    fi
    if [[ -n "$empty_dict_match" ]]; then
        v=$(printf '%s\n' "$v" \
            "$(format_violation "$file_path" '高' \
                'Use dict() for empty dict, not {}' <<<"$empty_dict_match")")
    fi
    if [[ -n "$empty_list_match" ]]; then
        v=$(printf '%s\n' "$v" \
            "$(format_violation "$file_path" '高' \
                'Use list() for empty list, not []' <<<"$empty_list_match")")
    fi

    v=$(append "$v" "$(check_python_pep8 "$file_path")")
    v=$(append "$v" "$(check_python_typing "$file_path")")
    v=$(append "$v" "$(check_python_none_union "$file_path")")
    v=$(append "$v" "$(check_python_def_parens "$file_path")")
    v=$(append "$v" "$(check_python_call_parens "$file_path")")
    v=$(append "$v" "$(check_python_closing_paren_arrow "$file_path")")
    v=$(append "$v" "$(check_python_multiline_ternary "$file_path")")
    v=$(append "$v" "$(check_python_section_delimiters "$file_path")")

    echo "$v"
}

check_python_pep8() {
    local file_path="$1"
    if ! command -v pycodestyle >/dev/null 2>&1; then
        return
    fi
    pycodestyle --max-line-length=79 "$file_path" 2>/dev/null || true
}

check_python_typing() {
    local file_path="$1"
    _grep_violation "$file_path" \
        '(typing\.(Dict|List|Tuple|Union|Any)\b|from typing import[^#]*(Dict|List|Tuple|Union|Any))' \
        '高' 'Use dict[]/list[]/tuple[]/Optional[X], not typing.*'
}

check_python_none_union() {
    local file_path="$1"
    _grep_violation "$file_path" \
        '\w+\s*\|\s*None\b|None\s*\|\s*\w+' \
        '高' 'Use Optional[X], not X | None'
}

check_python_def_parens() {
    local file_path="$1"
    _grep_violation "$file_path" \
        '^\s*(async\s+)?def\s+\w+\($' \
        '中' 'First arg must be on same line as def funcname('
}

check_python_call_parens() {
    local file_path="$1"
    if ! command -v python3 >/dev/null 2>&1; then
        return
    fi
    local match
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
    if [[ -n "$match" ]]; then
        format_violation "$file_path" '中' \
            'First arg must be on same line as (; align continuation to opening (' \
            <<<"$match"
    fi
}

check_python_closing_paren_arrow() {
    local file_path="$1"
    _grep_violation "$file_path" \
        '^\)\s*->' \
        '中' 'Closing ) at col 0; must align to opening ( column'
}

check_python_multiline_ternary() {
    local file_path="$1"
    _grep_violation "$file_path" \
        '^\s+if\s.+\belse\b' \
        '中' 'Multi-line ternary; rewrite as if/else block'
}

check_python_section_delimiters() {
    _check_file_section_delimiters "$1" 79 '# '
}


# ------------------------------------------------------------------------------
# ----------------------------- TS / JS checks ---------------------------------
# ------------------------------------------------------------------------------
check_typescript() {
    local file_path="$1"
    local v=''
    v=$(append "$v" "$(check_class_component "$file_path")")
    v=$(append "$v" "$(check_any_type "$file_path")")
    v=$(append "$v" "$(check_typescript_interface_props "$file_path")")
    v=$(append "$v" "$(check_untyped_params "$file_path")")
    v=$(append "$v" "$(check_typescript_section_delimiters "$file_path")")
    echo "$v"
}

check_javascript() {
    local file_path="$1"
    local v=''
    v=$(append "$v" "$(check_class_component "$file_path")")
    v=$(append "$v" "$(check_mixed_modules "$file_path")")
    v=$(append "$v" "$(check_pascalcase_function "$file_path")")
    v=$(append "$v" "$(check_console_logs "$file_path")")
    v=$(append "$v" "$(check_module_level_let "$file_path")")
    v=$(append "$v" "$(check_javascript_section_delimiters "$file_path")")
    echo "$v"
}

# Shared TS/JS checks
check_class_component() {
    _grep_violation "$1" \
        'extends\s+(React\.)?(Component|PureComponent)\b' \
        '高' 'Use function component, not class component'
}

check_any_type() {
    _grep_violation "$1" \
        ':\s*any\b|<any>' \
        '高' "Avoid 'any' type; use explicit type"
}

# TS-only
check_typescript_interface_props() {
    _grep_violation "$1" \
        '^(export\s+)?interface\s+\w+(Props|State)\b' \
        '高' "Use 'type' instead of 'interface' for Props/State"
}

check_untyped_params() {
    local file_path="$1"
    local match
    match=$(grep -nE 'function\s+\w+\s*\([^)]*[^):]\s*[^):]\)' \
        "$file_path" 2>/dev/null | grep -v '//' || true)
    if [[ -n "$match" ]]; then
        format_violation "$file_path" '中' 'Type all function parameters' \
            <<<"$match"
    fi
}

# JS-only
check_mixed_modules() {
    local file_path="$1"
    local has_require has_import
    has_require=$(grep -cE '\brequire\s*\(' "$file_path" 2>/dev/null \
        || echo 0)
    has_import=$(grep -cE '\bimport\s+' "$file_path" 2>/dev/null \
        || echo 0)
    if [[ "$has_require" -gt 0 && "$has_import" -gt 0 ]]; then
        printf '%s:0: -> [高] Mixed module systems: do not mix require() and import/export\n' \
            "$file_path"
    fi
}

check_pascalcase_function() {
    local file_path="$1"
    local match
    match=$(grep -nP \
        '^(export\s+)?(async\s+)?function\s+(?!use)[A-Z][a-zA-Z0-9_]*\s*\([^)]*\)\s*\{' \
        "$file_path" 2>/dev/null || true)
    if [[ -n "$match" ]]; then
        format_violation "$file_path" '中' \
            'Non-component function with PascalCase; use camelCase or add use prefix for hooks' \
            <<<"$match"
    fi
}

check_console_logs() {
    _grep_violation "$1" \
        '\bconsole\.(log|warn|error|info)\s*\(' \
        '低' 'Remove console.log/debug statements from production code'
}

check_module_level_let() {
    _grep_violation "$1" \
        '^(export\s+)?(let|var)\s+\w+' \
        '中' 'Avoid mutable module-level state (let/var at top level)'
}

check_typescript_section_delimiters() {
    _check_file_section_delimiters "$1" 80 '// '
}

check_javascript_section_delimiters() {
    _check_file_section_delimiters "$1" 80 '// '
}


# ------------------------------------------------------------------------------
# -------------------------------- Shell checks --------------------------------
# ------------------------------------------------------------------------------
check_shell() {
    local file_path="$1"
    local v=''
    v=$(append "$v" "$(check_shell_shebang "$file_path")")
    v=$(append "$v" "$(check_shell_eval "$file_path")")
    v=$(append "$v" "$(check_shell_strict_mode "$file_path")")
    v=$(append "$v" "$(check_shell_cd_error "$file_path")")
    v=$(append "$v" "$(check_shell_section_delimiters "$file_path")")
    echo "$v"
}

check_shell_shebang() {
    local file_path="$1"
    local head
    head=$(head -n1 "$file_path" 2>/dev/null || true)
    if ! grep -qE '^#!/bin/(bash|sh)' <<<"$head"; then
        printf '%s:1: -> [高] Missing shebang (#!/bin/bash or #!/bin/sh) at top of file\n' \
            "$file_path"
    fi
}

check_shell_eval() {
    local file_path="$1"
    local match
    match=$(grep -nE '\beval\s' "$file_path" 2>/dev/null \
        | grep -vE '^\s*#' || true)
    if [[ -n "$match" ]]; then
        format_violation "$file_path" '高' \
            'Avoid eval; use indirect expansion or arrays instead' \
            <<<"$match"
    fi
}

check_shell_strict_mode() {
    local file_path="$1"
    local head
    head=$(head -n1 "$file_path" 2>/dev/null || true)
    if grep -qF '#!/bin/bash' <<<"$head"; then
        if ! grep -qE '^set\s+-euo?\s+pipefail' "$file_path" 2>/dev/null; then
            printf '%s:0: -> [中] Missing set -euo pipefail at start of script\n' \
                "$file_path"
        fi
    elif grep -qF '#!/bin/sh' <<<"$head"; then
        if ! grep -qE '^set\s+-eu' "$file_path" 2>/dev/null; then
            printf '%s:0: -> [中] Missing set -eu at start of script\n' \
                "$file_path"
        fi
    fi
}

check_shell_cd_error() {
    local file_path="$1"
    local match
    match=$(grep -nE '^\s*cd\s+\S+' "$file_path" 2>/dev/null \
        | grep -v '||' | grep -v '&&' || true)
    if [[ -n "$match" ]]; then
        format_violation "$file_path" '中' \
            "cd without error check; use 'cd dir || exit 1'" \
            <<<"$match"
    fi
}

check_shell_section_delimiters() {
    _check_file_section_delimiters "$1" 80 '# '
}


# ------------------------------------------------------------------------------
# ------------------------- Shared section delimiters --------------------------
# ------------------------------------------------------------------------------
_check_file_section_delimiters() {
    local file_path="$1" expected="$2" prefix="$3"
    local -a lines=()
    local line total

    while IFS= read -r line || [[ -n "$line" ]]; do
        lines+=("$line")
    done < "$file_path"
    total=${#lines[@]}

    for ((i = 0; i < total; i++)); do
        line="${lines[$i]}"
        # Skip non-comment lines
        if [[ "$line" != "$prefix"* ]]; then
            continue
        fi

        local content
        content="${line#"$prefix"}"
        local len
        len=$(printf '%s' "$line" | wc -c)

        if grep -qE '^-+' <<<"$content"; then
            # Pure dash line or named header
            if [[ "$len" -ne "$expected" ]]; then
                printf '%s:%s: -> [高] Section delimiter must be exactly %s chars (got %s)\n' \
                    "$file_path" "$((i + 1))" "$expected" "$len"
            fi
            if grep -qE ' $' <<<"$line"; then
                printf '%s:%s: -> [高] Section delimiter has trailing space\n' \
                    "$file_path" "$((i + 1))"
            fi
            # Named header check (line 2 of 3-line block)
            if grep -qE '^--+ [^ -].*[^ -] --+$' <<<"$content"; then
                local left_dashes right_dashes left_len right_len diff
                left_dashes="${content%% *}"
                right_dashes="${content##* }"
                left_len=$(printf '%s' "$left_dashes" | wc -c)
                right_len=$(printf '%s' "$right_dashes" | wc -c)
                diff=$((left_len - right_len))
                if [[ "$diff" -lt 0 ]]; then diff=$((-diff)); fi
                if [[ "$diff" -gt 1 ]]; then
                    printf '%s:%s: -> [高] Section header hyphens unbalanced: left=%s, right=%s\n' \
                        "$file_path" "$((i + 1))" "$left_len" "$right_len"
                fi
                if [[ "$right_len" -lt "$left_len" ]]; then
                    printf '%s:%s: -> [高] Section header: right side should have more hyphens than left\n' \
                        "$file_path" "$((i + 1))"
                fi
            fi
        else
            # Non-dash comment line — check if it's improperly flanked
            if [[ -n "$content" ]]; then
                local prev_idx=$((i - 1)) next_idx=$((i + 1))
                local prev_content next_content is_flanked=0
                if [[ "$prev_idx" -ge 0 && "$next_idx" -lt "$total" ]]; then
                    prev_content="${lines[$prev_idx]#"$prefix"}"
                    next_content="${lines[$next_idx]#"$prefix"}"
                    if grep -qE '^--+$' <<<"$prev_content" \
                        && grep -qE '^--+$' <<<"$next_content"; then
                        is_flanked=1
                    fi
                fi
                if [[ "$is_flanked" -eq 1 ]]; then
                    printf '%s:%s: -> [高] Missing section delimiter dashes (expected %s chars)\n' \
                        "$file_path" "$((i + 1))" "$expected"
                fi
            fi
        fi
    done
}


# ------------------------------------------------------------------------------
# -------------------------------- Utilities -----------------------------------
# ------------------------------------------------------------------------------
append() {
    local base="$1" addition="$2"
    if [[ -z "$addition" ]]; then
        echo "$base"
    elif [[ -z "$base" ]]; then
        echo "$addition"
    else
        printf '%s\n%s\n' "$base" "$addition"
    fi
}

_grep_violation() {
    local file_path="$1" pattern="$2" severity="$3" message="$4"
    local match
    match=$(grep -nE "$pattern" "$file_path" 2>/dev/null || true)
    if [[ -n "$match" ]]; then
        format_violation "$file_path" "$severity" "$message" <<<"$match"
    fi
}


main "$@"


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
