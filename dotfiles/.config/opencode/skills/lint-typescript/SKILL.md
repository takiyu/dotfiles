---
name: lint-typescript
description: Run TypeScript/React lint tools, then check the project's custom coding rules. Trigger when the user asks to lint TypeScript or React code, or audit coding-rule compliance.
---

# TypeScript Lint

Run the available TypeScript/React lint tools, then check the project's custom
coding rules and fix every violation iteratively. Findings use the severity
labels `[高]` / `[中]` / `[低]`.

## Principles

- **Tool-first.** Run the detected lint tools before hand-checking the custom
  rules.
- **Fix iteratively.** Fix every reported violation, then re-run until the
  result is clean.
- **Severity labels.** Report each finding as `[高]`, `[中]`, or `[低]`.
- **Japanese output.** Write the summary and the findings in Japanese; keep
  this skill body and the banners in English.
- **Visible progress.** Announce the run and finish with the summary table
  defined under "Report".

## Workflow

1. **Detect tools** — choose ONE branch below.

   **Branch A** — Makefile has a `lint:` target:

   ```sh
   make lint 2>&1
   ```

   **Branch B** — package.json has a `lint` script:

   ```sh
   npm run lint 2>&1
   ```

   **Branch C** — fallback to ESLint + tsc:

   ```sh
   npx eslint --fix . 2>&1 || true
   command -v tsc >/dev/null 2>&1 && tsc --noEmit 2>&1 || true
   ```

2. **Review custom rules** — scan the source files against "Rules" below and
   fix every violation iteratively.

3. **Report** — print the summary defined under "Report".

## Rules

**[高] Must fix:**
- Class component used (must use function component)
- Props not typed
- User-visible string not wrapped in `t()` (i18n)
- `any` type used
- `interface` used for props/state (use `type`)
- camelCase variable that should be snake_case (non-function, non-component)
- PascalCase function that is not a React component or hook
- Section delimiters wrong (see AGENTS.md Delimiter spacing): 80 chars, `// ` prefix
- Trailing comma after the last element (see AGENTS.md)
- File does not end with exactly 3 lines of `// -----------------------------------------------------------------------------` (80 chars)

**[中] Should fix:**
- Test file not named `test_*.ts` / `test_*.tsx`
- React Bootstrap not used where a UI component is needed
- Missing English comment at start of non-trivial code block
- Sentence-style names with prepositions
- Hook defined outside component but missing `use` prefix
- Mutable module-level state
- Unused import
- Inline if-else (ternary) expressions (allowed only in the JSX/HTML markup part of React components, e.g. `{cond ? <A /> : <B />}`)
- Unnecessary line breaks; pack multiple statements or conditions per line when within 100-char limit

**[低] Nice to fix:**
- `console.log` left in production code
- Long function exceeding ~50 lines

## Report

Print the summary when the run finishes:

```markdown
## ✅ Lint complete (TypeScript)

| 項目 | 件数 |
| --- | --- |
| 対象ファイル | <N> |
| [高] Critical | <N> |
| [中] Important | <N> |
| [低] Minor | <N> |
```

Print a progress line before each fix pass:

`🔁 <i> 回目 — [高] <a> / [中] <b> / [低] <c> 残り`
