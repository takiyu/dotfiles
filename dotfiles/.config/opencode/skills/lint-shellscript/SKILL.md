---
name: lint-shellscript
description: Run shell lint tools, then check the project's custom coding rules. Trigger when the user asks to lint shell scripts or audit coding-rule compliance.
---

# Shell Script Lint

Run the available shell lint tools, then check the project's custom coding
rules and fix every violation iteratively. Findings use the severity labels
`[高]` / `[中]` / `[低]`.

## Principles

- **Tool-first.** Run the detected lint tools before hand-checking the custom
  rules.
- **Fix iteratively.** Fix every reported violation, then re-run until the
  result is clean.
- **Severity labels.** Report each finding as `[高]`, `[中]`, or `[低]`.
- **Japanese output.** Write the summary and the findings in Japanese; keep
  this skill body and the banners in English.
- **Visible progress.** Announce the run and finish with the summary table
  defined under "Report".

## Workflow

1. **Detect tools** — choose ONE branch below.

   **Branch A** — Makefile has a `lint:` target:

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

2. **Review custom rules** — scan the source files against "Rules" below and
   fix every violation iteratively.

3. **Report** — print the summary defined under "Report".

## Rules

**[高] Must fix:**
- Missing `#!/bin/bash` or `#!/bin/sh` shebang
- `eval` usage
- Unquoted variable expansions where word-splitting/globbing is dangerous
- `source` without `set -e` or error handling
- POSIX `sh` scripts using arrays, `[[ ]]`, `$'...'`
- Section delimiters wrong (see AGENTS.md Delimiter spacing): 80 chars, `# ` prefix
- Trailing comma after the last element (see AGENTS.md)
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
- Inline if-else (`&&` / `||` used as ternary replacement)
- Unnecessary line breaks; pack multiple statements or conditions per line when within length limits

## Report

Print the summary when the run finishes:

```markdown
## ✅ Lint complete (Shell Script)

| 項目 | 件数 |
| --- | --- |
| 対象ファイル | <N> |
| [高] Critical | <N> |
| [中] Important | <N> |
| [低] Minor | <N> |
```

Print a progress line before each fix pass:

`🔁 <i> 回目 — [高] <a> / [中] <b> / [低] <c> 残り`
