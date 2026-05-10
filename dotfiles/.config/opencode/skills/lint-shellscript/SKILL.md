---
name: lint-shellscript
description: Run shell script lint tools (make lint → bash -n), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash
---

# ------------------------------------------------------------------------------
# ------------------------------ Lint Shell Script -----------------------------
# ------------------------------------------------------------------------------

# Run linters and custom coding rule checks for shell script files.


# ------------------------------------------------------------------------------
# ------------------------------- Step 1: Detect -------------------------------
# ------------------------------------------------------------------------------

# Detect available tools and project configuration.

```sh
# Detect available shell tools
echo "=== tools available ==="
for t in make bash sh; do
    command -v "$t" >/dev/null 2>&1 \
        && echo "$t: $(command -v $t)" || echo "$t: not found"
done

# Check for Makefile lint target
echo "=== Makefile lint target ==="
grep -c '^lint:' Makefile 2>/dev/null \
    && echo "Makefile has lint target" || echo "no Makefile lint"
```


# ------------------------------------------------------------------------------
# ------------------------------- Step 2: Run ----------------------------------
# ------------------------------------------------------------------------------

# Choose ONE branch based on Step 1 detection results.

**Branch A** — Makefile has `lint:` target:
```sh
# Run make lint target
make lint 2>&1
```

**Branch B** — fallback to bash syntax check + shellcheck (if available):
```sh
# Syntax check with bash/sh and optionally shellcheck
for f in $(find . -maxdepth 5 -name '*.sh' ! -path '*/.git/*'); do
    echo "=== checking $f ==="
    bash -n "$f" 2>&1 || true
    command -v shellcheck >/dev/null 2>&1 \
        && shellcheck "$f" 2>&1 || true
done
```

**Do NOT run `apt install`, `pip install`, or use any tool not found in Step 1.**


# ------------------------------------------------------------------------------
# --------------------------- Step 3: Find Files -------------------------------
# ------------------------------------------------------------------------------

# Discover and display relevant source files for review.

```sh
# Find relevant shell script files for review
FILES=$(find . -maxdepth 5 -name '*.sh' \
    ! -path '*/.git/*' ! -path '*/node_modules/*')
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```


# ------------------------------------------------------------------------------
# ---------------------- Step 4: Custom Rules Check ----------------------------
# ------------------------------------------------------------------------------

# Review Steps 2–3 output. Report violations the tools **cannot** auto-fix.
# One per line in this format:

```
<file>:<line>: [<severity>] <message>
```

**[高] Must fix:**
- Missing `#!/bin/bash` or `#!/bin/sh` shebang at top of file
- `eval "$(some_command)"` or `eval $var` — use indirect expansion or arrays instead
- Unquoted variable expansions (`$VAR` instead of `"$VAR"`) in contexts where word-splitting/globbing is dangerous
- `source` command without `set -e` or error handling
- Unsupported shell features in POSIX `sh` scripts (arrays `()`, `[[ ]]`, `$'...'`, etc.)
- Named section header (`# ---` x3 with centered name) not preceded by
  exactly 2 blank lines, OR followed by any blank lines — must have 2
  empty lines before and 0 after (code starts immediately)
- Separator block (`# ---` x3 with no name, e.g. interface/impl divider or
  EOF terminator) not preceded by exactly 2 blank lines, OR not followed
  by exactly 1 blank line — must have 2 empty lines before and 1 after
- Section delimiter line length or alignment wrong: every line must be
  exactly 80 chars with no trailing whitespace. Lines 1 and 3 must be
  `# ` + 78 `-`. Line 2 must center the section name with left and right
  `-` counts equal or differing by exactly 1 (right side gets the extra
  `-` when total is odd, never a trailing space)
- File does not end with exactly 3 lines of `# ------------------------------------------------------------------------------`
  (each 80 chars) with no trailing newline after the last line

**[中] Should fix:**
- Missing `set -euo pipefail` (bash) or `set -eu` (sh) at start of script
- Hardcoded paths where `$HOME`, `$XDG_CONFIG_HOME`, or environment variables should be used
- Script exceeding ~100 lines without function extraction
- `cd` without error check (`cd dir || exit 1`)
- Subshell abuse where command grouping `{}` would suffice
- No English comment at start of non-trivial code block

**[低] Nice to fix:**
- Inconsistent indentation (mix of tabs and spaces)
- Long pipeline chains exceeding ~3 stages without intermediate variables
- `echo` with unescaped special characters (use `printf` instead)


# ------------------------------------------------------------------------------
# --------------------------- Step 5: Print Summary ----------------------------
# ------------------------------------------------------------------------------

# Print final summary of lint results.

```
==================================================
 Lint complete (Shell Script)
==================================================
 Files checked : <N>
 [高] Critical  : <N>
 [中] Important : <N>
 [低] Minor     : <N>
==================================================
```


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
