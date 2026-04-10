# Task Rules
- Never give up until finished the task including behavior check and testing without relying on users. (important)
- Must check the code behavior by running or testing it. (important)
- When environment variable missing, search `.env` file and source it.
- After completing tasks, you must repeatedly check that the rules are being followed.
- Always use high reasoning effort for agent responses unless explicitly overridden.
- No git commit without user instructions.

# Coding Rules
## Must Do
- Follow existing code style (important)
- Use snake_case for files, variables: xxx_filename, xxx_dirname
- Keep structure simple, avoid deep nesting
- Extract pure functions outside classes
- Write comments in English to explain logic
- Add comments for complex logic blocks
- Add one-line English comment at the beginning of each code block
- Function dependencies should be clear (higher-level (caller) should be above; lower-level (callee) should be below)
- Import/Include should be at the top of the file
- Keep changes to a minimum and do not change irrelevant parts.
- Absolutely forbid duplicated code; similar functionality must be consolidated into common functions.
- Constants must be defined in `constants.*` files.
- Use semantically correct structure
- Write minimal, concise, and readable code
- Always write comparison operators with the left side smaller (use the "<" orientation)
- After implementing each feature, you must compile and test to confirm operation before proceeding to the next feature.

## Must Not Do
- Use class methods/global variables (avoid)
- Create too many methods

## Code Structure Template
```python
Import/include code here
...
 (2 empty lines)

# -----------------------------------------------------------------------------
# ------------------------------- Section Name --------------------------------
# ----------------------------------------------------------------------------- (no empty line)
Interface code here
...
 (2 empty lines)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
 (1 empty lines)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------------ Implementation -------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
 (1 empty lines)
# -----------------------------------------------------------------------------
# ------------------------------- Section Name --------------------------------
# ----------------------------------------------------------------------------- (no empty line)
Implementation code here
...
 (2 empty lines)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ----------------------------------------------------------------------------- (EOF)
```
`#` can be replaced (e.g. `//`) for other languages.

## Python Specifics
- Must use Poetry for virtual environment
- 79 chars/line, PEP 8 (important)
- PascalCase classes, snake_case functions/variables
- Use type hints, single quotes
- `import os.path as osp` not Path
- Use built-in generics (`dict[K, V]`, `list[X]`, `tuple[X, Y]`) and `Optional[X]` for type hints
- Align continuation lines to the opening delimiter (`(`, `[`, `{`), not hanging indent
- Pack multiple parameters/arguments per line within 79-char limit (do not use one-per-line unnecessarily)
- When return type doesn't fit on last parameter line, put `)` aligned with params then `-> Type:` on the same line


### Known Antipatterns — NEVER write these (失敗事例)
```python
# ❌ WRONG: def with ( alone at end of line
def func(
        arg1: str, arg2: int) -> None: ...
# ✅ CORRECT:
def func(arg1: str, arg2: int,
         more: bool = False) -> None: ...

# ❌ WRONG: function call with ( alone at end of line
result = my_func(
    arg1, arg2, arg3)
# ✅ CORRECT:
result = my_func(arg1, arg2,
                 arg3)

# ❌ WRONG: multi-line ternary expression
x = (value
     if condition else other)
# ✅ CORRECT:
if condition:
    x = value
else:
    x = other

# ❌ WRONG: closing ) at column 0 before ->
def func(
        arg1: str
) -> Type: ...
# ✅ CORRECT:
def func(arg1: str,
         arg2: int) -> Type: ...
```

## TypeScript Specifics
- 2-space indent, 100 chars/line
- PascalCase classes, camelCase functions, snake_case variables
- Use React Bootstrap, i18n all text
- vitest: test_*.ts files

## Docker
- Use Dockerfile only (not docker-compose for single service)

## Commit Messages
- Feature/Fix/Docs/Style/Refactor/Test: description
