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
- **Loop to convergence.** Fix the reported issues, re-review from scratch, and
  stop only when nothing or only `[低]` issues remain, or when a round changes
  nothing.
- **Reviewable fixes.** Keep fixes unstaged so the user can inspect `git diff`;
  re-review with `git diff HEAD`.
- **Japanese output.** Write the interactive messages and the issue text in
  Japanese; keep this skill body and the banners in English.
- **Read-only git only.** Never run a git command that changes state: `add`
  (including `-N`, `-A`, `-u`), `restore`, `checkout`, `switch`, `stash`,
  `reset`, `rm`, `mv`, `clean`, `apply`, `commit`, `merge`, `rebase`,
  `cherry-pick`, and `update-index` are all forbidden. The only allowed git
  commands are read-only: `status`, `diff`, `log`, `show`, `ls-files`,
  `rev-parse`, and `blame`. Handle untracked files with `git ls-files --others`
  and read them directly; never stage them to expose them.

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
   `... [truncated: <N> chars total]`. Then, under a `Review Target` banner,
   print the stat, a blank line, and the change set.

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
   highest severity first, each using one of these labels:
   [高] <file>:<line> <problem> (why it is wrong / which rule)
   [中] ...
   [低] ...

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
   - When the status is `ISSUES`, at least one line starts with `[高]`, `[中]`,
     or `[低]`.
   - The reply is not a copy of the instructions or placeholder text.

   Re-invoke the subagent from step 4 up to 3 attempts total; if it still
   fails, print its raw reply and ask the user how to proceed.

6. **Fix and loop** — decide from the validated reply:

   - `STATUS: OK` → stop and go to step 7.
   - Only `[低]` issues → stop and go to step 7.
   - Otherwise → fix the `[高]` and `[中]` issues in the working tree only,
     following the loaded rules and keeping each change minimal, then return
     to step 2 for a fresh change set and review.

   Never touch the index while fixing; leave it exactly as the user left it.

   Stop early and go to step 7 when a round changes nothing or when 5 rounds
   have run, and say why.

7. **Report** — print the summary:

   ```
   ==================================================
    Auto-Review (Diff) complete
   ==================================================
    Rounds    : <N>
    Fixed     : <N>
    Remaining : <N> ([高] <a> / [中] <b> / [低] <c>)
    Status    : clean | trivial-only | capped
   ==================================================
   ```
