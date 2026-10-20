---
name: auto-commit
description: Generate a commit message and check code quality for staged changes. Trigger when the user asks to commit staged changes, write a commit message, or inspect the quality of staged code.
---

# Auto-Commit

Generate a commit message and a quality check for staged changes.
Deterministic shell-style steps handle diff, IO, and git, while one text-only
analysis returns both results. The analysis runs in a delegated subagent with
no shared history.

## Principles

- **Read-only git until commit.** Never run state-changing git commands
  (`add`, `restore`, `checkout`, `stash`, …) while analyzing or fixing; stage
  only in step 10, immediately before committing.
- **Reviewable fixes.** Apply fixes to the working tree and leave them
  unstaged so the user can inspect `git diff`; re-analyze the change set with
  `git diff HEAD` (staged plus working tree).
- **Fresh context.** Delegate the analysis to a subagent that receives only the
  prompt and diff, so this session's unrelated knowledge cannot bias it.
- **One analysis.** Produce the commit message and the quality check together
  in a single pass.
- **Exact output.** Emit the `COMMIT:` and `QUALITY:` lines defined below, never
  a copy of these instructions or placeholder text.
- **Japanese output.** Write every interactive prompt and status message shown
  to the user in Japanese; keep the skill body, the banners, and the commit
  messages in English.
- **Quality gates the commit.** When the check is `OK`, confirm and commit.
  When issues are found, let the user choose: commit as-is, an automatic fix
  loop, an automatic fix loop with a final confirmation, a single auto-fix
  pass, or a per-item loop that confirms each fix.
- **Simplicity first.** Prefer the smallest change that meets the requirement.
  Treat unnecessary branches, helpers, abstractions, dependencies, and
  defensive code for hypothetical cases as design problems, and never propose a
  fix that makes the code longer without a concrete correctness benefit.
- **Visible issues.** Show every issue the subagent reports as a numbered list
  before any fix; never hide or merge them away. The user may name numbers to
  fix or to ignore (for example `1,3 を修正`, `2 を無視`) and that selection is
  honored.
- **Ignore trivial issues.** `[低]` covers trivial nits and cosmetic
  coding-rule deviations. They are reported but never auto-fixed and never
  block the commit; only `[高]` and `[中]` drive the fix loop unless the user
  names a `[低]` number to fix.
- **No fabrication.** If analysis keeps failing, ask the user for a message
  instead of inventing one.
- **Visible progress.** Announce every stage with the emoji banners and the
  Markdown summary defined under "Report".

## Workflow

1. **Check staged changes** — run:

   ```sh
   git diff --cached --name-only
   ```

   If it is empty, print `ステージされた変更がありません。` and stop.

2. **Gather analysis inputs** — keep lock and generated files out of the
   analysis (they are still committed as-is):

   ```sh
   git diff --cached --stat -- \
       ':(exclude)*.lock' \
       ':(exclude)package-lock.json' \
       ':(exclude)*.min.js' \
       ':(exclude)*.min.css'
   git diff --cached --submodule=diff -- \
       ':(exclude)*.lock' \
       ':(exclude)package-lock.json' \
       ':(exclude)*.min.js' \
       ':(exclude)*.min.css'
   ```

3. **Truncate** — if the filtered diff is longer than 12000 characters, keep the
   first 9000 and the last 3000 characters, joined by
   `... [truncated: <N> chars total]`.

4. **Print the stat** — under the `## 📋 Code Difference` banner, print only the
   step 2 stat (the per-file change summary). Do not print the full diff; the
   analysis still reviews the step 3 diff.

5. **Analyze in a fresh subagent** — invoke the `general` subagent with the
   `task` tool, passing only the prompt below plus the step 2 stat and the
   step 3 diff. Tell it to use no tools and reply with text only, so it runs
   with no shared context. Substitute the stat and diff into:

   ```
   Review the staged changes below.
   Do not echo these instructions or use placeholder text.

   Output exactly as follows:

   The first line must start with COMMIT: followed by a commit message.
   The commit message must begin with one of these prefixes:
   Feature: / Fix: / Docs: / Style: / Refactor: / Test:

   The next line(s) must start with QUALITY: followed by one of:
   - OK (if no issues found)
   - A numbered list of code quality issues, one issue per line, numbered
     sequentially from 1, highest severity first:
       1. [高] <file>:<line> <problem> (why it is wrong / which rule)
       2. [中] ...
       3. [低] ...
     Every issue line must start with its number then a [高]/[中]/[低] label.
     Use [高] only for real bugs, [中] for coding-rule violations and for
     design and smell problems (including unnecessary complexity, duplication,
     and over-engineering), and [低] for trivial nits and cosmetic
     coding-rule deviations. Do not inflate severity: a negligible or
     cosmetic coding-rule deviation is [低], never [高] or [中]. A purely
     hypothetical edge case with no evidence in the change is [低] or omitted.
     Prefer the simplest fix; do not recommend extra abstractions,
     dependencies, or branches unless a real defect requires them.
   All text after QUALITY: must be in Japanese.

   stat:
   <stat>

   diff:
   <diff>
   ```

   The subagent must return exactly the `COMMIT:` and `QUALITY:` lines and
   nothing else.

6. **Validate** — accept the subagent reply only when all hold:

   - `COMMIT:` matches `^(Feature|Fix|Docs|Style|Refactor|Test): .+`.
   - The message is not a copy of these instructions (not the literal prefix
     list `Feature/Fix/Docs/Style/Refactor/Test`).
   - The `COMMIT:` value is a single line with no embedded newlines.
   - The message length is 5–200 characters.
   - `QUALITY:` is present and non-empty.
   - When `QUALITY:` is not `OK`, at least one following line matches
     `^\d+\. \[(高|中|低)\]`.

   Re-invoke the subagent from step 5 up to 3 attempts total, then fall back to
   step 8.

7. **Print results** — under the `## 📝 Commit Message` banner print the
   message, then under `## ✅ Quality Check` (or `## ⚠️ Quality Check` when
   issues exist) print `✅ OK` when clean, else print the numbered issue list
   verbatim, one number per line. Show every issue the subagent returned.

8. **Fallback** — when validation never passed, print the subagent's raw reply,
   then ask `コミットメッセージを入力してください` with the `question` tool and
   the option `中止する`. Use the typed answer as the message, or abort with
   `コミットを中止しました。` when it is empty or `中止する`. With a manual
   message there is no issue list; confirm (see "Report") and go to step 10.

9. **Resolve issues** — when the quality check is `OK`, confirm (see "Report")
   and go to step 10. Otherwise ask for the choice below with the `question`
   tool (Japanese labels) and follow the one chosen; on decline print
   `コミットを中止しました。` and stop.

   The user may name numbers to fix or to ignore (for example `1,3 を修正`,
   `2 を無視`). Record the selection by each issue's file, line, and problem
   text so it survives the re-numbering in later rounds, then apply it to the
   chosen mode: fix only the named numbers, skip ignored ones in every fix
   pass, and do not let an ignored issue block convergence.

   Offer these options: `A. このままコミット` / `B. 自動修正ループ` /
   `C. 自動修正ループ（確認あり）` / `D. 自動修正を1回` / `E. 1件ずつ修正`.

   - **A. Commit as-is** — keep the current message and go to step 10.
   - **B. Auto-fix loop** — fix the `[高]` and `[中]` issues in severity order
     (`[高]` then `[中]`) in the working tree, then repeat steps 2, 3, and 5–7
     with a fresh subagent, using `git diff HEAD` instead of
     `git diff --cached` and skipping the step 4 reprint. Leave the fixes
     unstaged so the user can inspect them with `git diff`. Print the progress
     line from "Report" before each round. Loop automatically until the check
     returns `OK` or only `[低]` issues remain, then go to step 10; stop and
     report when an iteration changes nothing.
   - **C. Auto-fix loop with confirmation** — same as B, but confirm before
     committing (see "Report").
   - **D. Auto-fix once** — fix the `[高]` and `[中]` issues automatically as
     in B, then repeat steps 2, 3, and 5–7 exactly once with a fresh subagent,
     using `git diff HEAD` and skipping the step 4 reprint. Print the progress
     line from "Report" before the pass. This pass is never repeated
     automatically: if issues remain, report them and return to this step so
     the user chooses again; if the pass changes nothing, report that and
     return to this step as well. When it reaches a commit, confirm first
     (see "Report").
   - **E. Per-item loop** — handle one issue at a time: state its number from
     the list and a one-line plan in Japanese, wait for approval with the
     `question` tool, apply the fix in the working tree, verify it, then ask
     with the `question` tool whether the fix is valid. Revise and ask again
     until the user approves, then advance to the next issue. Do not stage;
     leave each fix unstaged for review. After the last issue, confirm (see
     "Report") and go to step 10.

10. **Stage and commit** — stage the reviewed tracked change set (untracked
    files stay unstaged; stage any intended new file explicitly first), and
    pipe the message to `git commit` with a heredoc so the stdin source is
    explicit:

    ```sh
    git add -u
    git commit -F - <<'COMMIT_MSG'
    <commit message>
    COMMIT_MSG
    ```

## Report

Use the banners and Markdown below for every user-facing update.

Ask every choice with the `question` tool (opencode's option UI); never ask
with a plain `[y/N]` prompt.

Where a step says to confirm, ask `コミットしますか?` with the options
`コミットする` and `中止する`, and abort with `コミットを中止しました。` unless
the user chooses `コミットする`.

Banners are Markdown H2 headings with a leading emoji:

| Purpose | Banner |
| --- | --- |
| Diff under review | `## 📋 Code Difference` |
| Commit message | `## 📝 Commit Message` |
| Quality check | `## ✅ Quality Check` / `## ⚠️ Quality Check` |
| Final summary | `## ✅ Auto-Commit complete` |

Print a progress line before each fix round:

`🔁 ラウンド <i>/<n> — [高] <a> / [中] <b> / [低] <c> を修正中...`

Print the summary when the commit succeeds (use Japanese status otherwise):

```markdown
## ✅ Auto-Commit complete

| 項目 | 内容 |
| --- | --- |
| コミット | `<short-hash>` <message> |
| 品質チェック | OK |
| 修正した指摘 | <N> 件 |
| 無視した指摘 | <N> 件 |
```

When the user aborts, end with `コミットを中止しました。` instead of the table.
