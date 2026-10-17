---
name: auto-review-diff
description: Review the git diff regardless of staging (staged, unstaged, and untracked changes) against the AGENTS.md coding rules and sound software-engineering design, delegating each review to a fresh subagent and looping fixes until nothing or only trivial issues remain. Trigger when the user asks to review changes, review the diff, or audit modified files.
---

# Auto-Review (Diff)

Review the current git change set against the coding rules in `AGENTS.md` and
against sound software-engineering design, then fix the problems in a loop.
The change set is every difference from `HEAD` regardless of staging, plus
untracked files. Each review runs in a delegated subagent with no shared
history. The loop stops when a review finds nothing or only trivial issues.

## Principles

- **Fresh context.** Delegate each review to a subagent that receives only the
  rules and the change set, so this session's knowledge cannot bias it.
- **Staging-agnostic.** Review `git diff HEAD` (staged plus unstaged) and
  untracked files, so the result does not depend on what is staged.
- **Rules from AGENTS.md.** Judge coding style against the `AGENTS.md` coding
  rules, and judge design and implementation on general software-engineering
  grounds.
- **Read-only review.** The reviewer must not modify anything: allow only
  read-only tools (`read`, `grep`, `glob`) and forbid state-changing commands.
- **Severity labels.** Report every issue as `[高]`, `[中]`, or `[低]`, highest
  first, always with file and line.
- **Visible issues.** Print every issue the subagent reports as a numbered list
  under the findings banner before fixing anything; never hide or merge them
  away. By default fix `[高]` and `[中]`; when the user names numbers to fix or
  to ignore (for example `1,3 を修正`, `2 を無視`), follow that selection.
- **Loop to convergence.** Fix the reported issues, re-review from scratch, and
  stop only when nothing or only `[低]` issues remain, or when a round changes
  nothing.
- **Reviewable fixes.** Keep fixes unstaged so the user can inspect `git diff`;
  re-review with `git diff HEAD`.
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

2. **Collect the change set** — include staged and unstaged changes plus
   untracked files, keeping lock and generated files out of the review:

   ```sh
   git diff HEAD --stat -- \
       ':(exclude)*.lock' \
       ':(exclude)package-lock.json' \
       ':(exclude)*.min.js' \
       ':(exclude)*.min.css'
   git diff HEAD --submodule=diff -- \
       ':(exclude)*.lock' \
       ':(exclude)package-lock.json' \
       ':(exclude)*.min.js' \
       ':(exclude)*.min.css'
   git ls-files --others --exclude-standard -- \
       . \
       ':(exclude)*.lock' \
       ':(exclude)package-lock.json' \
       ':(exclude)*.min.js' \
       ':(exclude)*.min.css'
   ```

   Print each untracked file's content after the diff. Never stage the
   untracked files to expose them; read them directly. If there is no diff and
   no untracked file, print `レビュー対象の変更がありません。` and stop.

3. **Truncate and print** — if the change set is longer than 12000 characters,
   keep the first 9000 and the last 3000 characters, joined by
   `... [truncated: <N> chars total]`. Then, under the `## 📋 Review Target`
   banner, print the stat, a blank line, and the change set.

4. **Review in a fresh subagent** — invoke the `general` subagent with the
   `task` tool. Pass the rules and the change set, and allow read-only tools so
   it can read the affected files for context. Tell it to judge both
   coding-rule compliance and design/implementation quality, and to reply in
   exactly this format:

   ```
   Review the changes below.

   Judge the changes against two things:
   1. The AGENTS.md coding rules provided below.
   2. Sound software-engineering design and implementation: correctness,
      clear structure, no duplication, no dead code, sensible naming,
      error handling, and simplicity.

   Read the affected files with read-only tools (read, grep, glob) when more
   context is needed. Do not modify anything. Never run a git command that
   changes state: add (including -N, -A, -u), restore, checkout, switch, stash,
   reset, rm, mv, clean, apply, commit, merge, rebase, cherry-pick, and
   update-index are forbidden; read-only git (status, diff, log, show, ls-files,
   rev-parse, blame) is fine. Report only real problems, with file and line, and
   do not invent issues.
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
   Use [高] for rule violations and real bugs, [中] for design and smell
   problems, and [低] for trivial nits. Write all issue text in Japanese.

   coding rules:
   <rules>

   stat:
   <stat>

   diff:
   <diff>
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
     to step 2 for a fresh change set and review.

   Never touch the index while fixing; leave it exactly as the user left it.

   Stop early and go to step 8 when nothing left to fix remains, when a round
   changes nothing, or when 5 rounds have run, and say why.

8. **Report** — print the summary defined under "Report".

## Report

Use the banner and Markdown below for every user-facing update.

Banners are Markdown H2 headings with a leading emoji:

| Purpose | Banner |
| --- | --- |
| Change set under review | `## 📋 Review Target` |
| Findings | `## ⚠️ Issues` |
| Final summary | `## ✅ Auto-Review (Diff) complete` |

Print a progress line before each fix round:

`🔁 ラウンド <i>/<n> — [高] <a> / [中] <b> / [低] <c> を検出、修正中...`

Print the summary when the loop stops:

```markdown
## ✅ Auto-Review (Diff) complete

| 項目 | 値 |
| --- | --- |
| ラウンド | <N> |
| 修正 | <N> 件 |
| 無視 | <N> 件 |
| 残り | <N>（[高] <a> / [中] <b> / [低] <c>） |
| 状態 | 完了 / 軽微のみ / 上限到達 |
```
