---
name: auto-commit
description: Generates commit messages and checks code quality for staged changes.
---

# Auto-Commit

Generate a commit message and a quality check for the staged change set.
Deterministic shell-style steps handle diff, IO, and git, while one text-only
analysis returns both results. The analysis runs in a delegated subagent with
no shared history.

## Principles

- **Read-only git until commit.** Run no state-changing git command while
  analyzing or fixing; stage only in step 10, immediately before committing.
- **Reviewable fixes.** Apply fixes to the working tree and leave them
  unstaged; re-analyze the change set with `git diff HEAD` (staged plus working
  tree).
- **Fresh context.** Delegate the analysis to a subagent that receives only the
  prompt and diff, so this session's unrelated knowledge cannot bias it.
- **One analysis.** Produce the commit message and the quality check together
  in a single pass.
- **Exact output.** Require the subagent to emit only the `COMMIT:` and
  `QUALITY:` lines defined below, never a copy of the instructions or
  placeholder text.
- **Japanese output.** Write every interactive prompt and status message shown
  to the user in Japanese; keep the skill body, the banners, and the commit
  messages in English.
- **Quality gates the commit.** A failed check lets the user pick a resolution
  mode before the commit proceeds.
- **No fabrication.** If analysis keeps failing, ask the user for a message
  instead of inventing one.

## Workflow

1. **Check staged changes** — run this, and stop with
   `ステージされた変更がありません。` when it prints nothing:

   ```sh
   git diff --cached --name-only
   ```

2. **Gather analysis inputs** — keep lock and generated files out of both the
   analysis and the display; they are still committed as-is:

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

3. **Truncate** — when the filtered diff exceeds 12000 characters, replace it
   with its first 9000 and last 3000 characters joined by
   `... [truncated: <N> chars total]`, where `<N>` is the original length.

4. **Print the diff** — under a `Code Difference` banner, print the step 2 stat,
   a blank line, then the step 3 diff.

5. **Analyze in a fresh subagent** — invoke the `general` subagent with the
   `task` tool, passing only the prompt below. Tell it to use no tools and
   reply with text only, and replace the `<stat>` and `<diff>` placeholders with
   the step 2 stat and the step 3 diff before sending:

   ```
   Review the changes below.
   Do not echo these instructions or use placeholder text.

   Output exactly as follows:

   The first line must start with COMMIT: followed by a commit message.
   The commit message must begin with one of these prefixes:
   Feature: / Fix: / Docs: / Style: / Refactor: / Test:

   The next line(s) must start with QUALITY: followed by one of:
   - OK (if no issues found)
   - Japanese description of code quality issues, at most 10 lines.
   Severity labels [高]/[中]/[低] must be used if any issues exist.
   All text after QUALITY: must be in Japanese.

   stat:
   <stat>

   diff:
   <diff>
   ```

   Require exactly the `COMMIT:` and `QUALITY:` lines and nothing else.

6. **Validate** — accept the subagent reply only when all hold:

   - `COMMIT:` matches `^(Feature|Fix|Docs|Style|Refactor|Test): .+`.
   - The message is not a copy of the instructions (not the literal prefix
     list `Feature/Fix/Docs/Style/Refactor/Test`).
   - The `COMMIT:` value is a single line with no embedded newlines.
   - The message length is 5–200 characters.
   - `QUALITY:` is present and non-empty.

   Make at most 3 attempts in total (the first plus up to 2 re-invocations of
   the step 5 subagent). If all fail, skip to step 8; otherwise continue to
   step 7.

7. **Print results** — show the commit message under a
   `Generating commit message by LLM` banner, then the quality check under a
   `Quality Check by LLM` banner (`✅ <text>` when `OK`, else `⚠️ <text>`).

8. **Fallback** — when validation never passed, print the subagent's raw reply,
   then prompt `コミットメッセージを手動で入力してください (Enter で中止):`. Use
   the input as the message and go to step 10, or abort with
   `コミットを中止しました。` when the input is empty.

9. **Resolve issues** — on `OK`, skip to step 10. Otherwise present the modes
   below in Japanese as a lettered choice and read the user's typed letter
   (`A`–`E`); treat an unrecognized or empty answer as a decline, print
   `コミットを中止しました。`, and stop. The auto-fix modes (B, C, D) re-analyze
   with `git diff HEAD` (staged plus working tree) instead of
   `git diff --cached`.

   - **A. Commit as-is** — keep the current message and go to step 10.
   - **B. Auto-fix loop** — fix the issues in severity order (`[高]`, `[中]`,
     `[低]`), then re-run steps 2, 3, and 5–7 with a fresh subagent without
     reprinting the diff (no step 4). Loop until the check returns `OK`, then
     go to step 10; stop and report when an iteration changes nothing.
   - **C. Auto-fix loop with confirmation** — as B, but keep the step 10 prompt
     and wait for `y`/`Y` before committing.
   - **D. Auto-fix once** — same as B, but run a single analysis pass that
     reprints the diff (steps 2–7, with step 4), then return to this step and
     present the modes again; stop and report when the pass changes nothing.
   - **E. Per-item loop** — handle one issue at a time: state its index and a
     one-line plan in Japanese, wait for approval, apply the fix, show the fix
     diff (`git diff`), verify it (e.g. run tests or re-check the changed
     lines), then ask whether the fix is valid. Revise and ask again until the
     user answers OK, then advance to the next issue. After the last issue, go
     to step 10.

10. **Confirm, stage, and commit** — ask `⚠️ コミットしますか? [y/N]` and abort
    with `コミットを中止しました。` on anything but `y`/`Y`; mode B skips this
    confirmation. Then stage the tracked change set (untracked files stay
    unstaged; stage any intended new file explicitly first) and commit:

    ```sh
    git add -u
    git commit -F - <<'COMMIT_MSG'
    <commit message>
    COMMIT_MSG
    ```
