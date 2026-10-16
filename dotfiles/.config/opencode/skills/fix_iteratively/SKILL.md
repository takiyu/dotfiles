---
name: fix_iteratively
description: Fix a list of items one at a time with user approval per item. Trigger when the user asks to fix issues one by one, review each fix before committing, or work through a checklist incrementally.
---

# Fix Iteratively

Work through a list of fix items strictly one at a time. For each item:
propose a plan, wait for approval, apply the fix, wait for the user's check,
commit only after an explicit OK, then move to the next item. Never batch
items or proceed without approval.

## Principles

- **One item at a time.** Present, fix, verify, and commit a single item
  before touching the next. Never apply changes for a later item early.
- **Approve before editing.** Show the plan and wait for explicit approval
  before changing any code.
- **Propose in Japanese.** Write every plan, question, and progress report in
  Japanese (the skill body stays English).
- **Check before committing.** After the fix, stop and let the user review.
  Commit only on an explicit OK.
- **No unsolicited commits.** Commit only after the user confirms the item
  passes review; use the commit convention below.
- **Minimal scope.** Touch only what the current item requires; follow
  surrounding style and the rules in `AGENTS.md`.
- **Visible progress.** Keep the checklist and the per-item summary defined
  under "Report" up to date so the whole run is easy to scan.

## Workflow

1. **Collect items** — confirm the full list of fix items from the request.
   If none is given, inspect the target and propose a list, then wait for the
   user to confirm or reorder it before starting.
2. **Select one** — take the first (or user-chosen) item. State its number and
   a one-line summary.
3. **Propose a plan (in Japanese)** — describe the concrete fix approach:
   files, functions, and the intended change. Keep it small and reviewable.
   Write this proposal in Japanese.
4. **Wait for approval** — stop and ask the user to approve the plan. Do not
   edit yet. Revise the plan if the user requests changes, then ask again.
5. **Apply the fix** — after approval, make exactly the approved change.
6. **Verify** — run the relevant tests/build/typecheck and the matching
   `lint-<language>` skill, then review the diff. Report the result.
7. **Wait for the user's check** — stop and ask the user to review the fix.
   - OK → go to step 8.
   - Not OK → return to step 3 with the requested changes; re-verify.
8. **Commit** — on explicit OK, commit the item with a message in the form
   `Feature/Fix/Docs/Style/Refactor/Test: <description>` (or use the
   `auto-commit` skill if the user prefers it).
9. **Next item** — mark the item done, announce the remaining count, and
   return to step 2. Stop when all items are finished.

## Report

Use the Markdown below for every user-facing update.

Keep a task list visible, and update it when an item is committed:

```markdown
## 📌 Fix Progress

- [x] 1. <item>
- [ ] 2. <item>  ← 現在
- [ ] 3. <item>
```

After each committed item, print the per-item summary:

```markdown
## ✅ Item <i>/<n> committed

| 項目 | 内容 |
| --- | --- |
| 対象 | <summary> |
| 修正 | <what changed> |
| 検証 | pass / none / fail |
| コミット | `<short-hash>` <message> |
| 残り | <n - i> |
```
