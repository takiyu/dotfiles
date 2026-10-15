---
name: auto-commit
description: Generates commit messages and checks code quality for staged changes.
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
- **Quality gates the commit.** When the check is `OK`, go straight to step 10.
  When issues are found, let the user choose: commit as-is, an automatic fix
  loop, an automatic fix loop with a final confirmation, a single auto-fix
  pass, or a per-item loop that confirms each fix.
- **No fabrication.** If analysis keeps failing, ask the user for a message
  instead of inventing one.

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

4. **Print the diff** — under a `Code Difference` banner, print the stat, a
   blank line, then the step 3 diff. Print the same file set the analysis
   reviews, so the display matches the input.

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
   - Japanese description of code quality issues if any exist.
     Use multiple lines if needed.
   Severity labels [高]/[中]/[低] must be used if any issues exist.
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

   Re-invoke the subagent from step 5 up to 3 attempts total, then fall back to
   step 8.

7. **Print results** — show the commit message under a
   `Generating commit message by LLM` banner, then the quality check under a
   `Quality Check by LLM` banner (`✅ <text>` when `OK`, else `⚠️  <text>`).

8. **Fallback** — when validation never passed, print the subagent's raw reply,
   then prompt `コミットメッセージを手動で入力してください (Enter で中止):`. Use
   the input as the message, or abort with `コミットを中止しました。` when it is
   empty. With a manual message there is no issue list; go to step 10, which
   confirms before committing.

9. **Resolve issues** — when the quality check is `OK`, skip to step 10.
   Otherwise present the choices below in Japanese and follow the one chosen;
   on decline print `コミットを中止しました。` and stop.

   - **A. Commit as-is** — keep the current message and go to step 10.
   - **B. Auto-fix loop** — fix the reported issues in severity order (`[高]`
     then `[中]` then `[低]`) in the working tree, then repeat steps 2, 3, and
     5–7 with a fresh subagent, using `git diff HEAD` instead of
     `git diff --cached` and skipping the step 4 reprint. Leave the fixes
     unstaged so the user can inspect them with `git diff`. Loop automatically
     until the check returns `OK`; stop and report when an iteration changes
     nothing.
   - **C. Auto-fix loop with confirmation** — same as B, but do not skip the
     step 10 confirmation (ask `⚠️  コミットしますか? [y/N]` before committing).
   - **D. Auto-fix once** — fix all reported issues automatically as in B,
     then repeat steps 2, 3, and 5–7 exactly once with a fresh subagent, using
     `git diff HEAD` and skipping the step 4 reprint. This pass is never
     repeated automatically: if issues remain, report them and return to this
     step so the user chooses again; if the pass changes nothing, report that
     and return to this step as well. Any commit reached afterwards confirms at
     step 10.
   - **E. Per-item loop** — handle one issue at a time: state its index and a
     one-line plan in Japanese, wait for approval, apply the fix in the working
     tree, verify it, then ask the user whether the fix is valid. Revise and
     ask again until the user answers OK, then advance to the next issue. Do
     not stage; leave each fix unstaged for review. After the last issue, go to
     step 10, which confirms before committing.

10. **Confirm, stage, and commit** — ask `⚠️  コミットしますか? [y/N]` and
    abort with `コミットを中止しました。` on anything but `y`/`Y`. Mode B is the
    only exception: it commits without this prompt. Then stage the reviewed
    tracked change set (untracked files stay unstaged; stage any intended new
    file explicitly first), and pipe the message to `git commit` with a heredoc
    so the stdin source is explicit:

    ```sh
    git add -u
    git commit -F - <<'COMMIT_MSG'
    <commit message>
    COMMIT_MSG
    ```
