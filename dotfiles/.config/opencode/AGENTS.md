# Subagents
- Delegate independent subtasks via the task tool.
- Delegate exploration, research, and code review to subagents for parallelization.

# Task Rules
- Never give up until finished; include behavior checks and testing without relying on users.
- Must check code behavior by running or testing it.
- When environment variable missing, search `.env` file and source it.
- After completing tasks, repeatedly check that rules are being followed.
- Always use high reasoning effort unless explicitly overridden.
- No git commit without user instructions.
- **CRITICAL: NEVER use Chinese. Use English for thinking, Japanese for reports.**

# Coding Rules
## Must Do
- Follow existing code style (important)
- Use snake_case for files, variables (xxx_filename, xxx_dirname)
- Keep structure simple, avoid deep nesting
- Extract pure functions outside classes
- Write comments in English to explain logic
- Add comments for complex logic blocks
- Add one-line English comment at the beginning of each code block
- Function dependencies: higher-level (caller) above, lower-level (callee) below
- Import/Include at the top of the file
- Keep changes to a minimum; do not change irrelevant parts
- Absolutely forbid duplicated code; consolidate similar functionality into common functions
- Constants must be defined in `constants.*` files
- Use semantically correct structure
- Write minimal, concise, and readable code
- Always write comparison operators with the left side smaller (`<` orientation)
- After implementing each feature, compile and test before proceeding to the next

## Must Not Do
- Use class methods/global variables (avoid)
- Create too many methods

## Code Structure Template
```python
Import/include code here


# ------------------------------------------------------------------------------
# ------------------------------- Section Name ---------------------------------
# ------------------------------------------------------------------------------
Interface code here


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------ Implementation --------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------------------- Section Name ---------------------------------
# ------------------------------------------------------------------------------
Implementation code here


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
```
`#` can be replaced (e.g. `//`) for other languages.

## Language-specific
### Python
- 79 chars/line, PEP 8, Poetry environment
- PascalCase classes, snake_case functions/variables
- Type hints mandatory; never omit annotations (use `Optional[X]`, not `X | None`)
- `Any` discouraged but permitted when no other type exists
- `import os.path as osp` (not `pathlib.Path`)
- Built-in generics: `dict[K, V]`, `list[X]`, `tuple[X, Y]`, `Optional[X]`
- Align continuation lines to opening delimiter (`(`, `[`, `{`)
- Pack multiple parameters per line within 79-char limit
- `dict()` / `list()` for empty containers (not `{}` / `[]`)

### TypeScript
- 2-space indent, 100 chars/line
- PascalCase classes, camelCase functions, snake_case variables
- Use React Bootstrap, i18n all text
- vitest: `test_*.ts` files

### Docker
- Dockerfile only (not docker-compose for single service)

### Commit Messages
- `Feature/Fix/Docs/Style/Refactor/Test: description`
