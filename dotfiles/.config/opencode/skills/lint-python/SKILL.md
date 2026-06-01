---
name: lint-python
description: Run Python lint/format tools (make lint → poetry run isort/autopep8/pyright/flake8 → system pycodestyle/autopep8), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash, write, edit
---

# ------------------------------------------------------------------------------
# ------------------------------------ Lint ------------------------------------
# ------------------------------------------------------------------------------

# Run formatters, linters, and custom coding rule checks.


# ------------------------------------------------------------------------------
# ------------------------------- Step 1: Detect -------------------------------
# ------------------------------------------------------------------------------

# Detect available tools and project configuration.

```sh
# Detect available lint/format tools
echo "=== tools available ==="
for t in make poetry isort autopep8 pyright flake8 pycodestyle; do
    command -v "$t" >/dev/null 2>&1 \
        && echo "$t: $(command -v $t)" || echo "$t: not found"
done

# Check for Makefile lint target
echo "=== Makefile lint target ==="
grep -c '^lint:' Makefile 2>/dev/null \
    && echo "Makefile has lint target" || echo "no Makefile lint"

# Check for Poetry project configuration
echo "=== pyproject.toml ==="
grep '\[tool\.poetry\]' pyproject.toml 2>/dev/null \
    && echo "Poetry project" || echo "no pyproject.toml"
```


# ------------------------------------------------------------------------------
# ------------------------------- Step 2: Run ----------------------------------
# ------------------------------------------------------------------------------

# Choose ONE branch based on Step 1 detection results.

**Branch A** — Makefile has `lint:` target:
```sh
# Load environment variables if .env exists, then run make lint
[ -f .env ] && . ./.env || true
make lint 2>&1
```

**Branch B** — Poetry project (`[tool.poetry]` in pyproject.toml, `poetry` available):
```sh
# Run Poetry-based linting and formatting tools
poetry run isort . 2>&1 || true
poetry run autopep8 --in-place --recursive . 2>&1 || true
poetry run pyright 2>&1 || true
poetry run flake8 --exclude=.venv,*/.venv/* . 2>&1 || true
```

**Branch C** — system tools only (use only tools marked "available" in Step 1):
```sh
# Run system-level autopep8 formatter
command -v autopep8 >/dev/null 2>&1 \
    && autopep8 --in-place --recursive . 2>&1 || true

# Run system-level pycodestyle checker
command -v pycodestyle >/dev/null 2>&1 \
    && pycodestyle --max-line-length=79 \
       $(find . -maxdepth 5 -name '*.py' \
           ! -path '*/.venv/*' ! -path '*/__pycache__/*') 2>&1 || true
```

**Do NOT run `pip install`, create `.venv`, or use any tool not found in Step 1.**


# ------------------------------------------------------------------------------
# --------------------------- Step 3: Find Files -------------------------------
# ------------------------------------------------------------------------------

# Discover and display relevant source files for review.

```sh
# Find relevant source files for review
FILES=$(find . -maxdepth 5 \
    \( -name '*.py' -o -name '*.ts' -o -name '*.tsx' \) \
    ! -path '*/node_modules/*' ! -path '*/__pycache__/*' \
    ! -path '*/.git/*' ! -path '*/.venv/*' ! -path '*/dist/*')
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```


# ------------------------------------------------------------------------------
# ---------------------- Step 4: Custom Rules Check ----------------------------
# ------------------------------------------------------------------------------

# For each source file found in Step 3, scan line-by-line for custom-rule violations.
# **Do NOT collect all violations first** — follow this iterative process:
#
#   1. Pick ONE file.
#   2. Scan it and find the FIRST violation you can fix with `write` or `edit`.
#   3. Apply the fix immediately.
#   4. Re-read the corrected file with `read`.
#   5. Continue scanning the same file from the top for the next fixable issue.
#   6. When the file is clean, move to the next file.
#
# After every edit, re-run the relevant Step 2 linter if appropriate to verify.
# Report only violations that are **not** auto-fixable in this format (one per line):

```
<file>:<line>: [<severity>] <message>
```

**[高] Must fix:**
- Docstrings present (forbidden)
- Missing type hints on params or return type (never omit annotations)
- `typing.Dict/List/Tuple/Union` (use `dict[]/list[]/tuple[]/Optional[X]`)
- `typing.Any` — discouraged, but permitted when no other suitable type exists;
  never omit annotations to avoid `Any`
- String constants where an `enum` should be used
- `{}` for empty dict (use `dict()`), `[]` for empty list (use `list()`)
- `X | None` syntax (use `Optional[X]`)
- Private name (`_foo`) accessed from another module (excluding tests)
- Named section header (`# ---` x3 with centered name) not preceded by
  exactly 2 blank lines, OR followed by any blank lines — must have 2
  empty lines before and 0 after (code starts immediately)
- Separator block (`# ---` x3 with no name, e.g. interface/impl divider or
  EOF terminator) not preceded by exactly 2 blank lines, OR not followed
  by exactly 1 blank line — must have 2 empty lines before and 1 after
- Section delimiter line length or alignment wrong: every line must be
  exactly 79 chars with no trailing whitespace. Lines 1 and 3 must be
  `# ` + 77 `-`. Line 2 must center the section name with left and right
  `-` counts equal or differing by exactly 1 (right side gets the extra
  `-` when total is odd, never a trailing space)
- File does not end with exactly 3 lines of `# -----------------------------------------------------------------------------`
  (each 79 chars) with no trailing newline after the last line

**[中] Should fix:**
- camelCase function/variable name (must be snake_case)
- Fewer than 2 blank lines between top-level definitions
- Nesting deeper than 3 levels
- No English comment at start of non-trivial code block
- Class-level mutable state or module-level globals
- Continuation lines not aligned to opening delimiter (`(`, `[`, `{`);
  first argument **must** start on the same line as the opening `(` —
  never leave `(` alone at end of a `def` line or function call
- Closing `)` at column 0 before `-> ReturnType:` — must align to
  the column of the opening `(` (e.g. `                               ) -> T:`)
- `from pathlib import Path` (use `import os.path as osp`)
- Sentence-style names with prepositions (e.g. `load_and_parse`)
- Multi-line ternary expression (`x = (val\n    if cond else other)`);
  rewrite as `if/else` block instead
- Params or args split one-per-line when packing fits within 79 chars
  (usually a symptom of the hanging-indent `[中]` issue above)


# ------------------------------------------------------------------------------
# --------------------------- Step 5: Print Summary ----------------------------
# ------------------------------------------------------------------------------

# Print final summary of lint results.

```
==================================================
 Lint complete
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

