---
name: fix_coding_style
description: Find and fix places that violate the AGENTS.md coding rules, keeping algorithms and behavior identical, and verify the result by re-running the project's tests and sample programs. Trigger when the user asks to align code with the coding rules, fix coding style, clean up rule violations, or reformat code to the project conventions.
---

# Fix Coding Style

Bring existing code in line with the coding rules in `AGENTS.md`. This is a
style-only pass: the algorithm, the control flow, the public API, and the
runtime behavior must stay identical. Anything that cannot be fixed without
changing behavior is reported as unresolved instead of rewritten.

## Principles

- **Style only.** Change how the code is written, never what it computes.
  Never touch data structures, public names, or messages. A rewrite that
  removes a ternary, flattens a nesting level, or deletes dead code is allowed
  only because the rules demand it, and only when it is behavior-equivalent
  and re-verified.
- **Behavior is locked.** Record a baseline before editing and re-run it after
  each file (or each small batch). A changed result reverts that file and
  becomes an unresolved item. Docstrings are documentation: replacing them
  with comments changes `__doc__` and that is allowed, unless a test asserts
  `__doc__`, in which case the docstring stays and becomes unresolved.
- **Rules from AGENTS.md.** Use the project's `AGENTS.md` when it exists,
  otherwise `~/.config/opencode/AGENTS.md`. Apply only its "Coding Rules"
  section (Must Do, Must Not Do, structure, language-specific, Git).
- **Exact templates.** Never invent decoration: no trailing `#`, no extra
  dashes. A Python block (79 chars per line) looks exactly like this:

  ```
  # -----------------------------------------------------------------------------
  # ---------------------------------- ansi.py ----------------------------------
  # -----------------------------------------------------------------------------
  ```

  The name is the file name. Build the line as `"# "` + dashes + `" "` + name
  + `" "` + dashes, so the content between the leading `# ` and the end is
  exactly width - 2 characters; when the width leaves an odd remainder, put
  the extra dash on the right. TypeScript uses 100 and other languages 80,
  with the same shape and the name re-centered by moving dashes between the
  two sides.

- **Whole-file structure.** Copy this layout, taken from the AGENTS.md code
  structure template. Existing license or copyright comments stay verbatim
  directly below the file header, even when they name a file slightly
  differently; the stale-path rule does not apply to license text:

  ```
  3-line file header            (top, nothing above it)
  license comment, if any
  imports
  interface code                (public functions, classes, constants)

  2 blank lines
  3-line separator block

  1 blank line
  5-line Implementation block
  implementation code           (private constants, classes, helpers)

  2 blank lines
  3-line separator block        (end of file, exactly once)
  ```

  The Implementation block is two separator lines, the centered word
  `Implementation`, and two more separator lines, with no blank line after it.
  A short file that is all interface code still gets the 3-line separator
  before the end, but no Implementation block. When every definition of a file
  sits inside an indented platform branch (`try:` / `else:`), the top-level
  layout cannot be applied: leave the file as it is and report it.

- **Sections are optional.** The file header, the Implementation block, and
  the end-of-file block are always present when they apply. Named sections are
  added only where the file already has or clearly needs distinct groups.
  Never wrap a single statement or a single class in its own section, and
  never give a section header to `__init__.py`-style re-export files.

- **Public API is frozen.** Never rename exported names, class attributes,
  CLI flags, environment variables, or wire formats, even when they break the
  naming rule. Never move public constants or classes into new
  `constants.*` / `enum.*` modules in code you do not own. Report them.
- **Comments are one line.** Replacing a docstring with a single-line English
  comment is required. One line always wins over completeness: keep the single
  most useful fact the code does not already show and drop the rest. Never
  keep a multi-line comment to preserve detail.
- **Minimal diff.** Fix only what a rule requires. No drive-by reformatting,
  no new abstractions, no reordering beyond the caller-above-callee rule, no
  touching unrelated lines, no dependency installs. Extract a shared helper
  only when the duplication or ternary rule requires it, keep it small, and
  name it with a leading underscore so it stays private.
- **Type hints.** Use `Any` when a precise type is not statically expressible
  (ctypes values, stream proxies); `Any` counts as annotated. Never write
  `Optional[Any]`. Annotate module-level state and module constants that hold
  objects, not immutable `str` / `int` constants. Leave a parameter
  unannotated and unresolved only when any annotation would be wrong. A lambda
  cannot carry annotations: leave it and report it as unresolved.
- **Line wrapping.** Continuation lines align to the character after the
  opening delimiter. When that leaves no room, put the first argument on the
  same line as the call and wrap the rest aligned there; never leave `(` alone
  at the end of a line with an arbitrary indent below it. When a call cannot
  fit at all, bind the arguments to a local tuple first and pass that. A list,
  tuple, or dict literal that cannot align after its bracket may keep the
  bracket at the end of the line and indent its items by 8 more spaces.
- **PEP 8 spacing.** Two blank lines between top-level definitions. Extra
  blank lines between methods are `[低]`: leave them alone. Do not add blank
  lines inside a function body.
- **One file at a time.** Finish and verify a file before starting the next.
- **Diff audit.** Before reporting, read the whole diff and confirm every hunk
  is a comment, a blank line, a delimiter, an annotation, a pure move, a line
  re-wrap, a dead-code removal, or a behavior-equivalent control-flow rewrite.
  Any other hunk is a bug in the pass.
- **Severity labels.** Use the mapping in "Severity", never inflate.
  Unresolved items are listed separately, never folded into the fix list.
- **Read-only git only.** Never run `add`, `restore`, `checkout`, `switch`,
  `stash`, `reset`, `rm`, `mv`, `clean`, `apply`, `commit`, `merge`, `rebase`,
  `cherry-pick`, or `update-index`. Use `status`, `diff`, `log`, `show`,
  `ls-files`, and `rev-parse` only.
- **Japanese output.** Write the interactive messages and the findings in
  Japanese; keep this skill body and the banners in English.
- **Ask with options.** Use the `question` tool for any prompt. In a
  non-interactive run (a delegated subagent, or a task that already
  authorizes fixes), skip the prompt and fix `[高]` and `[中]` by default.
- **Visible progress.** Announce each pass with the progress line and finish
  with the summary table defined under "Report".

## Workflow

1. **Load the rules** — read `AGENTS.md` (project first, else
   `~/.config/opencode/AGENTS.md`) and keep the "Coding Rules" section. Note
   the delimiter width per language: Python 79, TypeScript 100, others 80.

2. **Set the target** — use the files or folders the user named. When none are
   named, list code files under the current folder:

   ```sh
   find . -type f \
       -not -path '*/.git/*' \
       -not -path '*/node_modules/*' \
       -not -path '*/.venv/*' -not -path '*/venv/*' \
       -not -path '*/__pycache__/*' \
       -not -path '*/dist/*' -not -path '*/build/*' \
       -not -name '*.lock' -not -name 'package-lock.json' \
       -not -name '*.min.js' -not -name '*.min.css'
   ```

   Skip vendored, generated, binary, and data files. Print the list under the
   `## 📋 Style Targets` banner with the file count.

3. **Record the baseline** — before any edit, create a scratch directory and
   save a pristine copy of every target file, keeping its relative path:

   ```sh
   BASE=/tmp/opencode/${OPENCODE_PID:-session}
   mkdir -p "$BASE"
   WORK=$(mktemp -d "$BASE/fix_coding_style.XXXXXX")
   mkdir -p "$WORK/baseline/orig"
   cp --parents <file> ... "$WORK/baseline/orig"
   ```

   Then run the project's own checks and save the output:

   ```sh
   python3 -m unittest discover -p '*_test.py' > "$WORK/baseline/test.log" 2>&1
   python3 -m compileall -q . > "$WORK/baseline/compile.log" 2>&1
   ```

   Use the project's commands when it has them (`make test`, `npm test`,
   `npx tsc --noEmit`, `cargo test`, ...). When it ships demos, examples, or a
   CLI, capture their stdout too:

   ```sh
   python3 <demo> > "$WORK/baseline/<demo>.out" 2>&1
   ```

   Pick examples that exercise the edited code. A demo that uses randomness
   cannot be compared byte for byte: pin its seed or exclude it and say so.
   When a platform branch never runs on this machine, compare the old and new
   modules under a stub of that platform instead of leaving it unverified. If
   no check is possible, say so and fall back to `git diff` review plus
   `python3 -m compileall`; never proceed silently.

4. **Scan** — read each target file fully and list violations grouped by file,
   ordered highest severity first. Write each one as
   `<n>. [高|中|低] <file>:<line> <rule>: <problem>`. Check against this
   condensed rule list:

   *Structure*
   - 3-line file header at the top, named after the file, centered, no
     decoration.
   - License or copyright comment kept verbatim below the header.
   - Section headers with 2 blank lines before and none after, only where
     groups exist.
   - A 3-line separator, then the 5-line Implementation block, before private
     helpers; never a bare end-of-file block in the middle.
   - Exactly one 3-line end-of-file block, with 2 blank lines before it.
   - Public interface above the Implementation block; non-exported constants
     and classes below it. Never move a public definition down.
   - Imports at the top, except conditional imports inside `try:` / `else:`
     or a platform branch; caller declared above callee within a group.
   - Constants in `constants.*`, enums in `enum.*`, config in `config.*`.
     In third-party code, a public move is not fixable: report it.
   - Duplicated blocks; unused variables, constants, imports, and parameters.

   *Writing*
   - snake_case files, functions, variables; PascalCase classes.
   - No class methods for pure logic, no module-level mutable state.
   - Two blank lines between top-level definitions.
   - Nesting deeper than 3 levels; missing single-line English comment on a
     non-trivial block; multi-line comments.
   - Existing one-line comment wording that only reads awkwardly, without
     breaking a rule: `[低]`, leave it alone.
   - Inline or multi-line if-else expressions; comparison with the larger
     value on the left; trailing comma after the last element.
   - Statements that fit the line limit but are split across lines, and lines
     over the limit.
   - Trailing whitespace, `(` left alone at end of line.

   *Per language*
   - Python: 79 chars, type hints everywhere (including module-level state
     and module constants that hold objects), `Optional[X]`, `dict()` /
     `list()` for empties, built-in generics, `import os.path as osp`,
     no docstrings.
   - TypeScript: 100 chars, 2-space indent, camelCase functions, snake_case
     variables, `test_*.ts`, i18n for user-visible text.
   - Shell: 80 chars, top-to-bottom sections instead of functions.

   Mark every candidate fix that would change behavior (public rename, moving
   a public constant, reordering statements with side effects, formatting
   inside a string literal) as unresolved instead of a fix.

5. **Print the findings** — under `## ⚠️ Violations`, print the numbered list
   verbatim. Never hide or merge items. When there are none, print `OK`.

6. **Fix file by file** — for each file with `[高]` or `[中]` items:

   a. Apply the smallest edit that satisfies the rule, preserving every
      identifier, literal, and statement. Convert docstrings to one-line
      comments. Add a type hint only when it is correct; use `Any` for
      runtime-typed values.
   b. Verify: re-run the checks from step 3 whose output can be affected by
      this file. Running the whole saved set is fine and preferred over
      guessing which demo imports which module.
   c. On any mismatch, restore the file from
      `$WORK/baseline/orig/<path>`, record the item as unresolved, and say
      which check failed.
   d. Print the progress line.

   Fix `[低]` items only when the user asks for them; otherwise list them and
   leave them out of the unresolved list.

7. **Self-check** — after the last fix, before the report:

   ```sh
   git diff --check
   awk 'length > 79 {print FILENAME": "FNR": "length}' <python files>
   ```

   Then confirm by eye: every file starts with its 3-line header, ends with
   exactly one 3-line block and 2 blank lines before it, has at most one
   Implementation block, and has no other bare separator triple in the middle.
   Finish with the diff audit from "Principles".

8. **Re-scan** — repeat steps 4-7 until a pass finds no fixable `[高]` or
   `[中]` violations, a pass changes nothing, or 3 passes have run. A pass is
   one scan, whether or not it leads to fixes. Append each pass's findings to
   the same numbered list, so the report counts every item fixed across the
   whole run. Stop and say why.

9. **Report** — print the summary defined under "Report", including the
   unresolved items and the verification result.

## Severity

| Label | Items |
| --- | --- |
| `[高]` | Missing or misaligned file header or end-of-file block, docstrings, missing type hints, trailing commas, unused or dead code, code that no longer compiles |
| `[中]` | Missing or misplaced section header or separator block, constants not in `constants.*`, naming, spacing, nesting, module-level mutable state, comment style, ternary expressions, declaration order, duplication, lines over the limit, comparison orientation |
| `[低]` | Cosmetic nits: extra blank lines inside a class, punctuation, wording of an existing comment |

Third-party code that would need a public API move keeps the rule's label and
goes to the unresolved list. Report one unresolved item per symbol family
(for example all `AnsiFore` colour names) instead of one per name.
Unresolved items keep the label of the rule they violate.

## Report

Use the banners and Markdown below for every user-facing update.

| Purpose | Banner |
| --- | --- |
| Target list | `## 📋 Style Targets` |
| Findings | `## ⚠️ Violations` |
| Finished run | `## ✅ Fix Coding Style complete` |
| Stopped on failure or limit | `## ⚠️ Fix Coding Style stopped` |

Print a progress line before each fix pass, or before each file when a pass
fixes several files:

`🔁 パス <i>/3 — [高] <a> / [中] <b> / [低] <c> を検出、修正中...`

Print the summary when the pass loop stops:

```markdown
## ✅ Fix Coding Style complete

| 項目 | 値 |
| --- | --- |
| 対象ファイル | <N> |
| パス | <N> |
| 修正 | <N> 件（[高] <a> / [中] <b> / [低] <c>） |
| 未解決 | <N> 件 |
| 挙動検証 | pass / not available / fail |
| 状態 | 完了 / 上限到達 / 検証失敗 |
```

`パス` is the number of scan passes run, including the first. `修正` counts
every violation item fixed across all passes, grouped as in the findings list.

List unresolved items below the table with the rule, the file and line, and
the reason the fix would change behavior.
