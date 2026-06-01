---
name: auto-commit
description: Generates commit messages and checks code quality for staged changes. Prefer gcmA alias for fast use.
allowed-tools: bash
---

# Auto-Commit Workflow

1. Get staged diff: `git diff --cached --submodule=diff`
   - If empty: stop with "No staged changes."
2. In one analysis, produce both:
   - Commit message: `Feature/Fix/Docs/Style/Refactor/Test: <description>`
   - Quality check: `OK` or issues (≤10 lines, Japanese, `[高]/[中]/[低]`)
3. Print diff, message, and quality result.
4. Prompt user: `Commit? [y/N]`
   - `y`/`Y` → `git commit -m "<message>"`
   - Else → abort
