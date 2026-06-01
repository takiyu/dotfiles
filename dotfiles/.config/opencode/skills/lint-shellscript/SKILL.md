---
name: lint-shellscript
description: Run shell lint tools, then check custom coding rules. Severity labels in Japanese.
allowed-tools: bash, write, edit
---

# Shell Script Lint

Detect available tools (make, bash, sh), then choose ONE branch:

**Branch A** — Makefile has `lint:` target:
```sh
make lint 2>&1
```

**Branch B** — fallback to bash syntax check + shellcheck (if available):
```sh
for f in $(find . -maxdepth 5 -name '*.sh' ! -path '*/.git/*'); do
    echo "=== checking $f ==="
    bash -n "$f" 2>&1 || true
    command -v shellcheck >/dev/null 2>&1 \
        && shellcheck "$f" 2>&1 || true
done
```

Then scan custom rules (fix iteratively):

**[高] Must fix:**
- Missing `#!/bin/bash` or `#!/bin/sh` shebang
- `eval` usage
- Unquoted variable expansions where word-splitting/globbing is dangerous
- `source` without `set -e` or error handling
- POSIX `sh` scripts using arrays, `[[ ]]`, `$'...'`
- Section delimiters wrong (80 chars, 2 blank lines before named header, 1 after separator)
- File does not end with exactly 3 lines of `# ------------------------------------------------------------------------------` (80 chars)

**[中] Should fix:**
- Missing `set -euo pipefail` (bash) or `set -eu` (sh)
- Hardcoded paths where env vars should be used
- Script exceeding ~100 lines without function extraction
- `cd` without error check
- Subshell abuse where `{}` would suffice
- No English comment at start of non-trivial code block

**[低] Nice to fix:**
- Inconsistent indentation (tabs vs spaces)
- Long pipeline chains exceeding ~3 stages without intermediate variables
- `echo` with unescaped special characters (use `printf`)

Print summary:
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
