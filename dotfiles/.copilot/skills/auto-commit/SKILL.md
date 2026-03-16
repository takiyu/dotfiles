---
name: auto-commit
description: Stage-based auto-commit skill. Gets git staged diff, generates commit message (Feature/Fix/Docs/Style/Refactor/Test format), checks code quality, and commits only if quality is OK.
allowed-tools: bash
---

# Auto-Commit

Perform auto-commit workflow for staged git changes in the current directory.

## Steps

### 1. Get Staged Diff

Run:
```sh
git diff --cached --submodule=diff
```

If the output is empty, stop and report: "No staged changes to commit."

### 2. Display Diff

Print with section header:
```
------------------------------------------------------------------
------------------------- Code Difference ------------------------
------------------------------------------------------------------
```

### 3. Generate Commit Message

Analyze the diff and create a single-line English commit message.
Format: `Feature/Fix/Docs/Style/Refactor/Test: description`

Print with section header:
```
------------------------------------------------------------------
---------------- Generating commit message by LLM ----------------
------------------------------------------------------------------
 > <commit message>
```

### 4. Check Code Quality

Evaluate the diff against these criteria:
- Algorithmic correctness
- Security
- Maintainability
- Readability
- Harmony with surrounding code (most important)
- Absence of bugs

**Output format (Japanese):**
- If everything is correct: output exactly `OK`
- If there are issues: output each issue in Japanese with an importance label
  - `[高]` = high importance
  - `[中]` = medium importance
  - `[低]` = low importance
  - Keep it concise (1–10 lines total)

Print with section header:
```
------------------------------------------------------------------
---------------------- Quality Check by LLM ----------------------
------------------------------------------------------------------
 > <quality result>
```

### 5. Commit Decision

**Quality result is `OK`:**
- Run: `git commit -m "<generated commit message>"`

**Quality issues found:**
- Do NOT run `git commit`
- Report: "品質問題のためコミットを中止しました。問題を修正してから再試行してください。"
