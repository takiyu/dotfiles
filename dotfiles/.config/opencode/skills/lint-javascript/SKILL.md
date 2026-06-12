---
name: lint-javascript
description: Run JavaScript/React lint tools, then check custom coding rules. Severity labels in Japanese.
allowed-tools: bash, write, edit
---

# JavaScript Lint

Detect available tools (make, npm, npx, eslint), then choose ONE branch:

**Branch A** — Makefile has `lint:` target:
```sh
make lint 2>&1
```

**Branch B** — package.json has `lint` script:
```sh
npm run lint 2>&1
```

**Branch C** — fallback to direct ESLint:
```sh
npx eslint --fix . 2>&1 || true
```

Then scan custom rules (fix iteratively):

**[高] Must fix:**
- Class component used (must use function component)
- User-visible string not wrapped in `t()` (i18n)
- Mixed require() and import/export in same file
- camelCase variable that should be snake_case (non-function, non-component)
- PascalCase function that is not a React component or hook
- Section delimiters wrong (80 chars, `// ` prefix)
- Trailing comma after the last element in function args, list, dict, tuple, etc. (only allowed when the number of elements is extremely large; prefer fewer lines over multiple lines, avoid trailing comma even when allowed)
- File does not end with exactly 3 lines of `# ------------------------------------------------------------------------------` (80 chars)

**[中] Should fix:**
- React Bootstrap not used where a UI component is needed
- Missing English comment at start of non-trivial code block
- Sentence-style names with prepositions
- Hook defined outside component but missing `use` prefix
- Mutable module-level state
- Unused import / require
- Inline if-else (ternary) expressions
- Unnecessary line breaks; pack multiple statements or conditions per line when within length limits

**[低] Nice to fix:**
- `console.log` left in production code
- Long function exceeding ~50 lines

Print summary:
```
==================================================
 Lint complete (JavaScript)
==================================================
 Files checked : <N>
 [高] Critical  : <N>
 [中] Important : <N>
 [低] Minor     : <N>
==================================================
```
