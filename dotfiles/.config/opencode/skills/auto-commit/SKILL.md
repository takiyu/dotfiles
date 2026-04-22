---
name: auto-commit
description: Generates commit message (Feature/Fix/Docs/Style/Refactor/Test) and checks code quality for staged changes. For fast use, prefer the copilot_auto_commit.sh script (gcmA alias).
allowed-tools: bash
---

# Auto-Commit (Interactive)

Perform auto-commit workflow for staged git changes.

## Steps

### 1. Get Staged Diff

```sh
git diff --cached --submodule=diff
```

If empty: report "No staged changes." and stop.

### 2. Single-Pass Analysis

In **one analysis**, produce both:
- **Commit message**: `Feature/Fix/Docs/Style/Refactor/Test: <description>`
- **Quality check**: `OK` or issues in Japanese with `[高]/[中]/[低]` labels (≤10 lines)

Print:
```
------------------------------------------------------------------
------------------------- Code Difference ------------------------
------------------------------------------------------------------
<diff>
------------------------------------------------------------------
---------------- Generating commit message by LLM ---------------
------------------------------------------------------------------
 > <commit message>
------------------------------------------------------------------
---------------------- Quality Check by LLM ----------------------
------------------------------------------------------------------
 > <quality result>
```

### 3. User Confirmation

```sh
read -p "Commit? [y/N]: " _r && echo "$_r"
```

- `y`/`Y` → Step 4
- else → "コミットを中止しました。" and stop

### 4. Commit

```sh
git commit -m "<commit message from Step 2>"
```
