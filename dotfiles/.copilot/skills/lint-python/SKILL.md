---
name: lint-python
description: Run Python lint/format tools (make lint → poetry run isort/autopep8/pyright/flake8 → system pycodestyle/autopep8), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash
---

# Lint

Run formatters, linters, and custom coding rule checks.

## Steps

### 1. Detect project setup

```sh
echo "=== tools available ==="
for t in make poetry isort autopep8 pyright flake8 pycodestyle; do
    command -v "$t" >/dev/null 2>&1 \
        && echo "$t: $(command -v $t)" || echo "$t: not found"
done
echo "=== Makefile lint target ==="
grep -c '^lint:' Makefile 2>/dev/null \
    && echo "Makefile has lint target" || echo "no Makefile lint"
echo "=== pyproject.toml ==="
grep '\[tool\.poetry\]' pyproject.toml 2>/dev/null \
    && echo "Poetry project" || echo "no pyproject.toml"
```

### 2. Run tools — choose ONE branch based on Step 1

**Branch A** — Makefile has `lint:` target:
```sh
[ -f .env ] && . ./.env || true
make lint 2>&1
```

**Branch B** — Poetry project (`[tool.poetry]` in pyproject.toml, `poetry` available):
```sh
poetry run isort . 2>&1 || true
poetry run autopep8 --in-place --recursive . 2>&1 || true
poetry run pyright 2>&1 || true
poetry run flake8 --exclude=.venv,*/.venv/* . 2>&1 || true
```

**Branch C** — system tools only (use only tools marked "available" in Step 1):
```sh
command -v autopep8 >/dev/null 2>&1 \
    && autopep8 --in-place --recursive . 2>&1 || true
command -v pycodestyle >/dev/null 2>&1 \
    && pycodestyle --max-line-length=79 \
       $(find . -maxdepth 5 -name '*.py' \
           ! -path '*/.venv/*' ! -path '*/__pycache__/*') 2>&1 || true
```

**Do NOT run `pip install`, create `.venv`, or use any tool not found in Step 1.**

### 3. Show auto-fix diff

```sh
git diff --stat 2>/dev/null || true
```

### 4. Find and show source files

```sh
FILES=$(git diff --name-only HEAD 2>/dev/null | grep -E '\.(py|ts|tsx)$')
if [ -z "$FILES" ]; then
    FILES=$(find . -maxdepth 5 \
        \( -name '*.py' -o -name '*.ts' -o -name '*.tsx' \) \
        ! -path '*/node_modules/*' ! -path '*/__pycache__/*' \
        ! -path '*/.git/*' ! -path '*/.venv/*' ! -path '*/dist/*')
fi
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```

### 5. Check custom coding rules

Review Steps 2–4 output. Report violations the tools **cannot** auto-fix.
One per line in this format:

```
<file>:<line>: [<severity>] <message>
```

**[高] Must fix:**
- Docstrings present (forbidden)
- `typing.Dict/List/Tuple/Union/Any` (use `dict[]/list[]/tuple[]/Optional[X]`)
- Missing type hints on params or return type
- String constants where an `enum` should be used
- `{}` for empty dict (use `dict()`), `[]` for empty list (use `list()`)
- `X | None` syntax (use `Optional[X]`)

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

**[低] Nice to fix:**
- Params or args split one-per-line when packing fits within 79 chars
  (usually a symptom of the hanging-indent `[中]` issue above)

### 6. Print summary

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
