---
name: lint-python
description: Run Python lint tools, then check custom coding rules. Severity labels in Japanese.
allowed-tools: bash, write, edit
---

# Python Lint

Detect available tools (make, poetry, isort, autopep8, pyright, flake8, pycodestyle) and choose ONE branch:

**Branch A** — Makefile has `lint:` target:
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

Then review source files and scan custom rules (fix iteratively):

**[高] Must fix:**
- Docstrings present
- Missing type hints on params or return
- `typing.Dict/List/Tuple/Union` (use `dict[]/list[]/tuple[]/Optional[X]`)
- `typing.Any` — allowed when no other type exists; never omit annotations to avoid it
- String constants where an enum should be used
- `{}` for empty dict (use `dict()`), `[]` for empty list (use `list()`)
- `X | None` syntax (use `Optional[X]`)
- Private name accessed from another module (excluding tests)
- Section delimiters wrong (79 chars, 2 blank lines before named header, 1 after separator)
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

Print summary:
```
==================================================
 Lint complete (Python)
==================================================
 Files checked : <N>
 [高] Critical  : <N>
 [中] Important : <N>
 [低] Minor     : <N>
==================================================
```
