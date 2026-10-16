---
name: lint-python
description: Run Python lint tools, then check the project's custom coding rules. Trigger when the user asks to lint Python code or audit coding-rule compliance.
---

# Python Lint

Run the available Python lint tools, then check the project's custom coding
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
   [ -f .env ] && . ./.env; make lint 2>&1
   ```

   **Branch B** — Poetry project (`[tool.poetry]` in pyproject.toml):

   ```sh
   poetry run isort . 2>&1 || true
   poetry run autopep8 --in-place --recursive . 2>&1 || true
   poetry run pyright 2>&1 || true
   poetry run flake8 --exclude=.venv,*/.venv/* . 2>&1 || true
   ```

   **Branch C** — system tools only:

   ```sh
   command -v autopep8 >/dev/null 2>&1 \
       && autopep8 --in-place --recursive . 2>&1 || true
   command -v pycodestyle >/dev/null 2>&1 \
       && pycodestyle --max-line-length=79 \
          $(find . -maxdepth 5 -name '*.py' \
              ! -path '*/.venv/*' ! -path '*/__pycache__/*') 2>&1 || true
   ```

2. **Review custom rules** — scan the source files against "Rules" below and
   fix every violation iteratively.

3. **Report** — print the summary defined under "Report".

## Rules

**[高] Must fix:**
- Docstring forbidden (use English comments instead)
- Missing type hints on params or return
- `typing.Dict/List/Tuple/Union` (use `dict[]/list[]/tuple[]/Optional[X]`)
- `typing.Any` — allowed when no other type exists; never omit annotations to avoid it
- String constants where an enum should be used
- `{}` for empty dict (use `dict()`), `[]` for empty list (use `list()`)
- `X | None` syntax (use `Optional[X]`)
- Private name accessed from another module (excluding tests)
- Section delimiters wrong (see AGENTS.md Delimiter spacing): 79 chars, `# ` prefix
- Trailing comma after the last element (see AGENTS.md)
- File does not end with exactly 3 lines of `# -----------------------------------------------------------------------------` (79 chars)

**[中] Should fix:**
- camelCase function/variable (must be snake_case)
- Fewer than 2 blank lines between top-level definitions
- Nesting deeper than 3 levels
- No English comment at start of non-trivial code block
- Class-level mutable state or module-level globals
- Continuation lines not aligned to opening delimiter; never leave `(` alone at end of line
- Closing `)` at column 0 before `-> ReturnType:`
- `from pathlib import Path` (use `import os.path as osp`)
- Sentence-style names with prepositions
- Multi-line ternary expression
- One-per-line params when packing fits within 79 chars
- Inline if-else (ternary) expression (including single-line)
- Unnecessary line breaks; pack multiple statements or conditions per line when within 79-char limit

## Report

Print the summary when the run finishes:

```markdown
## ✅ Lint complete (Python)

| 項目 | 件数 |
| --- | --- |
| 対象ファイル | <N> |
| [高] Critical | <N> |
| [中] Important | <N> |
| [低] Minor | <N> |
```

Print a progress line before each fix pass:

`🔁 <i> 回目 — [高] <a> / [中] <b> / [低] <c> 残り`
