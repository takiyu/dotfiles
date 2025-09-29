# Task Rules
- Never give up until finished the task including behavior check and testing without relying on users. (important)
- Must check the code behavior by running or testing it. (important)
- When envirommnet variable missing, search `.env` file and source it.

# Coding Rules
## Must Do
- Follow existing code style (important)
- Remove trailing whitespace, 2 lines between functions
- Use snake_case for files, variables: xxx_filename, xxx_dirname
- Keep structure simple, avoid deep nesting
- Extract pure functions outside classes
- Write comments in English to explain logic
- Add comments for complex logic blocks
- Function dependencies should be clear (from top to bottom)
- Import/Include should be at the top of the file
- Keep changes to a minimum and do not change irrelevant parts.
- Use enum instead of string constants.
- Absolutely forbid duplicated code; similar functionality must be consolidated into common functions.
- Constants must be defined in `constants.*` files.
- After implementing each feature, you must compile and test to confirm operation before proceeding to the next feature.

## Must Not Do
- Use docstrings (unnecessary)
- Use class methods/global variables (avoid)
- Create too many methods
- Deep indentation

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
`#` can be replaced with `//` for other languages.

# Task Rules
- Never give up until finished the task including behavior check and testing without relying on users. (important)
- Must check the code behavior by running or testing it. (important)
- When envirommnet variable missing, search `.env` file and source it.

# Coding Rules
## Must Do
- Follow existing code style (important)
- Remove trailing whitespace, 2 lines between functions
- Use snake_case for files, variables: xxx_filename, xxx_dirname
- Keep structure simple, avoid deep nesting
- Extract pure functions outside classes
- Write comments in English to explain logic
- Add comments for complex logic blocks
- Function dependencies should be clear (from top to bottom)
- Import/Include should be at the top of the file
- Keep changes to a minimum and do not change irrelevant parts.
- Use enum instead of string constants.

## Must Not Do
- Use docstrings (unnecessary)
- Use class methods/global variables (avoid)
- Create too many methods
- Deep indentation

## Code Structure Template
```python
# -----------------------------------------------------------------------------
# ------------------------------- Section Name --------------------------------
# ----------------------------------------------------------------------------- (no empty line)
Interface code here
...
 (2 empty lines)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------------ Implementation -------------------------------
# -----------------------------------------------------------------------------
# ----------------------------------------------------------------------------- (no empty line)
Implementation code here
...
 (2 empty lines)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
EOF
```

## Python Specifics
- Poetry for virtual environment
- 4-space indent, 79 chars/line, PEP 8 (important)
- PascalCase classes, snake_case functions/variables
- Use type hints, single quotes
- `import os.path as osp` not Path
- No typing.Dict/List/Tuple/Union/Any and object for typing
- pytest: tests/ dir, test_*.py files

## TypeScript Specifics
- 2-space indent, 100 chars/line
- PascalCase classes, camelCase functions, snake_case variables
- Function components, type all props
- Use React Bootstrap, i18n all text
- vitest: test_*.ts files

## Docker
- Use Dockerfile only (not docker-compose for single service)

## Commit Messages
- Feature/Fix/Docs/Style/Refactor/Test: description

## Python Specifics
- Poetry for virtual environment
- 4-space indent, 79 chars/line, PEP 8
- PascalCase classes, snake_case functions/variables
- Use type hints, single quotes
- `import os.path as osp` not Path
- No typing.Dict/List/Tuple/Union/Any and object for typing
- pytest: tests/ dir, test_*.py files

## TypeScript Specifics
- 2-space indent, 100 chars/line
- PascalCase classes, camelCase functions, snake_case variables
- Function components, type all props
- Use React Bootstrap, i18n all text
- vitest: test_*.ts files

## Docker
- Use Dockerfile only (not docker-compose for single service)

## Commit Messages
- Feature/Fix/Docs/Style/Refactor/Test: description
