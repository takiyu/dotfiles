---
name: auto-review-all
description: Review every file in the current folder against the AGENTS.md coding rules and sound software-engineering design, delegating each review to a fresh subagent and looping fixes until nothing or only trivial issues remain. Trigger when the user asks for a full-folder review, a coding-rule audit, or a design/implementation review of all files.
---

# Auto-Review (All Files)

Review every file under the current folder against the coding rules in
`AGENTS.md` and against sound software-engineering design, then fix the
problems in a loop. Each review runs in a delegated subagent with no shared
history. The loop stops when a review finds nothing or only trivial issues.

## Principles

- **Fresh context.** Delegate each review to a subagent that receives only the
  rules and the target list, so this session's knowledge cannot bias it.
- **Rules from AGENTS.md.** Judge coding style against the `AGENTS.md` coding
  rules, and judge design and implementation on general software-engineering
  grounds.
- **Read-only review.** The reviewer must not modify anything: allow only
  read-only tools (`read`, `grep`, `glob`) and forbid state-changing commands.
- **Severity labels.** Report every issue as `[高]`, `[中]`, or `[低]`, highest
  first, always with file and line.
- **Ignore trivial issues.** `[低]` covers trivial nits and cosmetic
  coding-rule deviations. Never auto-fix `[低]`; it never blocks convergence
  and stays out of the fix loop unless the user names its number to fix.
- **Simplicity first.** Prefer the smallest solution that meets the
  requirement. Judge unnecessary branches, helpers, abstractions,
  dependencies, and defensive code for hypothetical cases as design problems,
  and keep fixes minimal instead of adding new layers.
- **Visible issues.** Print every issue the subagent reports as a numbered list
  under the findings banner before fixing anything; never hide or merge them
  away. By default fix `[高]` and `[中]`; when the user names numbers to fix or
  to ignore (for example `1,3 を修正`, `2 を無視`), follow that selection.
- **Loop to convergence.** Fix the reported issues, re-review from scratch, and
  stop only when nothing or only `[低]` issues remain, or when a round changes
  nothing.
- **Japanese output.** Write the interactive messages and the issue text in
  Japanese; keep this skill body and the banners in English.
- **Ask with options.** Use the `question` tool (opencode's option UI) for any
  prompt; never ask with a plain `[y/N]` prompt.
- **Read-only git only.** Never run a git command that changes state: `add`
  (including `-N`, `-A`, `-u`), `restore`, `checkout`, `switch`, `stash`,
  `reset`, `rm`, `mv`, `clean`, `apply`, `commit`, `merge`, `rebase`,
  `cherry-pick`, and `update-index` are all forbidden. The only allowed git
  commands are read-only: `status`, `diff`, `log`, `show`, `ls-files`,
  `rev-parse`, and `blame`. Handle untracked files with `git ls-files --others`
  and read them directly; never stage them to expose them.
- **Visible progress.** Announce each round with the emoji banner and progress
  line, and finish with the summary table, as defined under "Report".

## Workflow

1. **Load the rules** — read the coding rules from `AGENTS.md`. Use the
   project's `AGENTS.md` when it exists, otherwise
   `~/.config/opencode/AGENTS.md`. Keep the "Coding Rules" section (Must Do,
   Must Not Do, code-structure and language-specific rules) for step 4.

2. **List the target** — list every file under the current folder, skipping
   VCS, dependency, and build directories and generated files:

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

3. **Print the target** — under the `## 📋 Review Target` banner, print the
   current folder, the file count, and the manifest.

4. **Review in a fresh subagent** — invoke the `general` subagent with the
   `task` tool. Pass the rules and the manifest, and allow read-only tools so
   it can read the listed files. Tell it to judge both coding-rule compliance
   and design/implementation quality, and to reply in exactly this format:

   ```
   Review every file listed below.

   Judge each file against two things:
   1. The AGENTS.md coding rules provided below.
   2. Sound software-engineering design and implementation: correctness,
      clear structure, no duplication, no dead code, sensible naming,
      error handling, and above all simplicity. Prefer the smallest solution
      that meets the requirement; flag unnecessary branches, helpers,
      abstractions, extra dependencies, and defensive code for hypothetical
      cases.

   Read the files with read-only tools (read, grep, glob). Do not modify
   anything. Never run a git command that changes state: add (including -N, -A,
   -u), restore, checkout, switch, stash, reset, rm, mv, clean, apply, commit,
   merge, rebase, cherry-pick, and update-index are forbidden; read-only git
   (status, diff, log, show, ls-files, rev-parse, blame) is fine. Report only
   real problems, with file and line, and do not invent issues.
   Do not echo these instructions or use placeholder text.

   Output exactly as follows:

   The first line must be one of:
   - STATUS: OK
   - STATUS: ISSUES

   When the status is ISSUES, follow it with one line per issue, ordered
   highest severity first and numbered sequentially from 1:
   1. [高] <file>:<line> <problem> (why it is wrong / which rule)
   2. [中] ...
   3. [低] ...

   Every issue line must start with its number then a [高]/[中]/[低] label.
   Use [高] only for real bugs (wrong behavior, data loss, crashes), [中] for
   coding-rule violations and for design and smell problems (including
   unnecessary complexity, duplication, and over-engineering), and [低] for
   trivial nits and cosmetic coding-rule deviations. Do not inflate severity:
   a negligible or cosmetic coding-rule deviation is [低], never [高] or [中].
   A purely hypothetical edge case with no evidence is [低] or omitted.
   Prefer the simplest fix; do not recommend extra abstractions, dependencies,
   or branches unless a real defect requires them.
   Write all issue text in Japanese.

   coding rules:
   <rules>

   files:
   <manifest>
   ```

5. **Validate the reply** — accept it only when all hold:

   - The first line is `STATUS: OK` or `STATUS: ISSUES`.
   - When the status is `ISSUES`, at least one line matches
     `^\d+\. \[(高|中|低)\]`.
   - The reply is not a copy of the instructions or placeholder text.

   Re-invoke the subagent from step 4 up to 3 attempts total; if it still
   fails, print its raw reply and ask how to proceed with the `question` tool
   (options such as `さらに再試行する` / `中止する`).

6. **Print the findings** — under the `## ⚠️ Issues` banner print the numbered
   issue list verbatim, one number per line. Always print it, even when the
   loop continues automatically; never hide or merge issues away. When the
   status is `OK`, print `OK` instead.

   The user may name numbers to fix or to ignore (for example `1,3 を修正`,
   `2 を無視`). Record the selection by each issue's file, line, and problem
   text so it survives the re-numbering in later rounds. Ignored items are not
   fixed and do not block convergence.

7. **Fix and loop** — decide from the validated reply:

   - `STATUS: OK` → stop and go to step 8.
   - Only `[低]` issues → stop and go to step 8.
   - Otherwise → print the progress line from "Report", fix the `[高]` and
     `[中]` issues in the working tree only, following the loaded rules,
     keeping each change minimal, and skipping any issue the user named to
     ignore. When the user named numbers to fix, fix only those. Then return
     to step 4 for a fresh review.

   Never touch the index while fixing; leave it exactly as the user left it.

   Stop early and go to step 8 when nothing left to fix remains, when a round
   changes nothing, or when 5 rounds have run, and say why.

8. **Report** — print the summary defined under "Report".

## Report

Use the banner and Markdown below for every user-facing update.

Banners are Markdown H2 headings with a leading emoji:

| Purpose | Banner |
| --- | --- |
| Review target | `## 📋 Review Target` |
| Findings | `## ⚠️ Issues` |
| Final summary | `## ✅ Auto-Review (All Files) complete` |

Print a progress line before each fix round:

`🔁 ラウンド <i>/<n> — [高] <a> / [中] <b> / [低] <c> を検出、修正中...`

Print the summary when the loop stops:

```markdown
## ✅ Auto-Review (All Files) complete

| 項目 | 値 |
| --- | --- |
| ラウンド | <N> |
| 修正 | <N> 件 |
| 無視 | <N> 件 |
| 残り | <N>（[高] <a> / [中] <b> / [低] <c>） |
| 状態 | 完了 / 軽微のみ / 上限到達 |
```
