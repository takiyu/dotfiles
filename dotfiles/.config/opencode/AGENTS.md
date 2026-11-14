# Task Rules
- Never give up until finished; must check code behavior by running or testing it without relying on users.
- When environment variable missing, search `.env` file and source it.
- After completing tasks, re-check the result against these rules before reporting.
- Always use high reasoning effort unless explicitly overridden.
- Do not blindly apply reviewer or subagent findings; judge whether each is appropriate before changing code.
- When the target project changes, call `session_move` to its working directory before reading, editing, running commands, starting builds, or changing configuration.
- Do not treat the previous directory as the project root after the target project changes.
- Stop and ask when the target working directory is unclear, missing, or cannot be moved to.
- No git commit without user instructions.
- A commit instruction covers only that single commit; never commit again unless the user explicitly asks for another commit.
- Never infer commit permission from earlier requests, from "review and commit" on a previous change, or from finishing work. Never commit, amend, reset, or revert unless the current request explicitly asks for it.
- **CRITICAL: NEVER use Chinese. Use English for thinking, Japanese for reports.**
- Delete temporary/scratch files created during the task when they are no longer needed (work files under /tmp, autosaves, downloads, build artifacts, etc.).
- Put session scratch files under /tmp/opencode/$OPENCODE_PID/ so unrelated sessions do not mix. Do not read or modify other sessions' directories.
- Do not pollute the global environment outside /tmp during work (no writes outside /tmp and the project without explicit approval).
- Scope all work to the directory where OpenCode is running and its subdirectories; without user instructions, do not read or reference anything outside it (parent directories, sibling projects, home, etc.). /tmp is exempt. During worktree work, the assigned worktree counts as the project root.
- Prohibited from installing tools casually. Ask the user before any installation or global environment change.
- If the user's request is incomplete or you have a better suggestion, consult with the user.
- When writing specifications or work logs, keep the document from bloating by removing unnecessary content.

# Documentation Rules
- Documents below follow the project conventions when a project `AGENTS.md` defines them; apply project-specific rules before these defaults.

## Spec Directory (`spec/`)
- Keep interface and operational contracts under `<root>/spec/`, one file per element (backend, frontend, devices, deployment) plus a `spec.md` index.
- Update the matching spec file in the same change whenever behavior, API, configuration, UI, or deployment changes.
- Keep the document map and update date in `spec.md` current.
- When spec and implementation disagree, fix the wrong side; never leave them out of sync.
- Keep specs to the current contract only; never record chronological history there.

## Change History (`HISTORY.md`)
- Append every change to `<root>/HISTORY.md` as a dated section, newest first.
- Write each entry as concise bullet lines: what changed and the resulting behavior, not implementation narration.
- Never duplicate history in specs.

## Project Rules (`AGENTS.md`)
- Keep `<root>/AGENTS.md` as the operational rule source for the project.
- When a rule violation or an inappropriate project state is found and fixed, add a short, general recurrence-prevention rule to it in the same change.
- State rules that prevent a whole class of violation; never record incident-specific details or concrete example values, and never restate existing rules.

# Coding Rules
## Must Do
- Code must always be kept clean, and any rule violations must be fixed immediately.
- Delete any unused variables or constants immediately.
- Follow existing code style (important)
- Use snake_case for files, variables (xxx_filename, xxx_dirname)
- Keep structure simple, avoid deep nesting
- Extract pure functions outside classes
- Write comments in English: every non-trivial code block must start with exactly one concise comment line
- Write comments as short noun phrases, not sentences, preferring gerund (-ing) verbs (e.g. `# Building the application`, `# Path dependencies`)
- Function declaration order MUST follow usage order, top-to-bottom: higher-level (caller) above, lower-level (callee) below. Group similar functions in the same section.
- Constants and classes that are not exported MUST be placed below the `Implementation` section separator (together with the implementation), never in the public/interface section above it. In Python, add `from __future__ import annotations` when a class must be moved below a function that annotates it.
- Import/Include at the top of the file
- Keep changes to a minimum; do not change irrelevant parts
- Absolutely forbid duplicated code; consolidate similar functionality into common functions
- Constants must be defined in `constants.*` files, except module-local constants prefixed with `_`, which may be defined below the `Implementation` section separator
  - For entrypoint files, constants may be declared at the top of the file
- Enumerations must be defined in `enum.*` files
- Represent kinds and types with enums, never with `str`
- Configuration control must be defined in `config.*` files
  - However, for single-script projects or extremely small projects, these may be written in the main script
- Write minimal, concise, and readable code with semantically correct structure
- Keep `README.md` short: a brief description and simple "How to use" steps only
- Put work history and detailed configuration in `HISTORY.md`, not `README.md`
- Always write comparison operators with the left side smaller (`<` orientation)
- Incrementally implement one feature at a time: write code, compile and test it, make it perfect, then move to the next. Do not write all code at once.
- Pack multiple statements, conditions, or parameters per line when within the length limit
- Never use inline if-else (ternary) expressions
- Write small shell scripts top-to-bottom with section separators; do not split them into functions
- Use the same comment for the same role across files
- Start every source file with a 3-line section header stating what the file does, e.g. `Korbato Scan Server Dockerfile`
- Align Markdown table borders to equal column widths, counting full-width characters as two columns

## Must Not Do
- Use class methods/global variables
- Create too many methods
- Put a bare file name (e.g. `auth.py`) as the top-of-file section header; describe the file's purpose instead
- Write multi-line comments (consecutive comment lines for one explanation); every comment, including existing ones, must be a single concise line
- Write long comment sentences, or chain clauses in a comment with `;` or similar punctuation. Keep every comment short and single-clause
- Leave stale paths or names in comments
- Trailing comma after the last element in function args, list, dict, tuple, etc. (allowed only when the number of elements is extremely large)

## Code Structure Template
```python
# -----------------------------------------------------------------------------
# ---------------------------- File Purpose Header ----------------------------
# -----------------------------------------------------------------------------
Import/include code here


# -----------------------------------------------------------------------------
# ------------------------------- Section Name --------------------------------
# -----------------------------------------------------------------------------
Interface code here


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------------ Implementation -------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ------------------------------- Section Name --------------------------------
# -----------------------------------------------------------------------------
Implementation code here


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
```
`#` can be replaced (e.g. `//`) for other languages.

Delimiter spacing:
- File header: at the very top (below a shebang if present), 0 blank lines after, and 1 blank line before a following section header
- Section header: 2 blank lines before, 0 after
- End-of-file separator block: 2 blank lines before
- Delimiter lines: Python 79 chars; TypeScript 100 chars; all others 80 chars

## Language-specific
### Python
- 79 chars/line, PEP 8, Poetry environment
- PascalCase classes, snake_case functions/variables
- Type hints mandatory; never omit annotations (use `Optional[X]`, not `X | None`)
- `Any` discouraged but permitted when no other type exists
- `import os.path as osp` (not `pathlib.Path`)
- Built-in generics: `dict[K, V]`, `list[X]`, `tuple[X, Y]`, `Optional[X]`
- Align continuation lines to opening delimiter (`(`, `[`, `{`)
- `dict()` / `list()` for empty containers (not `{}` / `[]`)

### TypeScript
- 2-space indent, 100 chars/line
- PascalCase classes, camelCase functions, snake_case variables
- Use React Bootstrap, i18n all text
- vitest: `test_*.ts` files
- Ternary expressions are allowed only in the JSX/HTML markup part of React components (e.g. `{cond ? <A /> : <B />}`)

### Docker
- Dockerfile only (not docker-compose for single service)

### Makefile
- Declare `.PHONY` once with all targets at the top, directly below the file header.
- Continue a long `.PHONY` target list with `\` instead of adding another `.PHONY` line.
- Prefix every target in a Docker section with `docker_`.
- 80 chars/line, recipes indented with tab.
- Section headers use dashed comment lines (`# --...--`), 2 blank lines before and 0 after; section name centered between dashes.
- Target name followed by `:`; all commands are one line each, separated by line breaks.
- Suppress echo with `@` for banner lines (`@echo "----"`); keep actual commands visible (no `@`).
- Split long shell commands with `\` line continuation, indented with tab.
- Group related targets into sections and keep an end-of-file separator block (2 blank lines before).
- Use `$(MAKE)` for recursive make.

## Git
- Do not use fast-forward merges by default. Use `git merge --no-ff` unless the
  user explicitly asks for a fast-forward merge.
- Name git branches as `<type>/<snake_case_summary>` (e.g. `feature/add_login`).

### Commit Messages
- Write commit messages in English.
- `Feature/Fix/Docs/Style/Refactor/Test/Security/Arrange: description`

## Parallel Work with Git Worktrees
### Starting
- Use worktrees when the user asks for parallel work, or when the task splits into independent parts.
- Announce the split before creating: task, branch, base commit, and file ownership.
- Confirm the split first when it is inferred rather than instructed.
- Create worktrees only with the OpenCode worktree tools, never with `git worktree`.
- Never reuse a branch across worktrees.
- Uncommitted changes in the original checkout are not carried over.

### Session and ownership
- Move the session into its worktree before any edit, and treat it as the project root.
- Keep the original checkout read-only during parallel work.
- Prefer one subagent per worktree, and pass it the path, base commit, scope, and test command.
- Never let one session or subagent edit two worktrees.
- Serialize tasks that touch the same files.
- Use a task-specific plan filename; never share one plan file across worktrees.

### Isolation
- Install, build, and test inside each worktree; never share generated directories.
- Use development `.env` values per worktree; never copy production secrets.
- Use unique ports, database names, container names, and cache names per worktree.
- Stop servers and background processes when finishing a worktree.
- Keep scratch files under `/tmp/opencode/$OPENCODE_PID/`, never in a worktree.

### Git safety
- Avoid `stash`, `gc`, `prune`, `config`, and hook edits.
- Never check out a branch held by another worktree.
- Regenerate lock files in one worktree only, or serialize the change.

### Review and integration
- Stop after verification and wait for the user to review the worktree changes.
- Never merge into the default branch before that review is approved.
- Merge one branch at a time with `git merge --no-ff` after fetching.
- Resolve conflicts in the integration worktree.

### Cleanup and reporting
- Report the path, branch, base commit, and test results.
- Remove only your own worktree, and only after merge and user confirmation.
- Check `worktree_list` before creating or removing; never touch another session's worktree.
- Stop and ask when creation fails, the branch exists, or the tree is dirty.
