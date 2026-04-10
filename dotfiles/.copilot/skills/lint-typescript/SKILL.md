---
name: lint-typescript
description: Run TypeScript/React lint tools (make lint → npm run lint → npx eslint --fix), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash
---

# Lint TypeScript

Run formatters, linters, and custom coding rule checks for TypeScript/React projects.

## Steps

### 1. Detect project setup

```sh
echo "=== tools available ==="
for t in make npm npx tsc; do
    command -v "$t" >/dev/null 2>&1 \
        && echo "$t: $(command -v $t)" || echo "$t: not found"
done
echo "=== Makefile lint target ==="
grep -c '^lint:' Makefile 2>/dev/null \
    && echo "Makefile has lint target" || echo "no Makefile lint"
echo "=== package.json lint script ==="
node -e "const p=require('./package.json'); console.log(p.scripts&&p.scripts.lint ? 'lint: '+p.scripts.lint : 'no lint script')" 2>/dev/null || echo "no package.json"
```

### 2. Run tools — choose ONE branch based on Step 1

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
command -v tsc >/dev/null 2>&1 \
    && tsc --noEmit 2>&1 || true
```

**Do NOT run `npm install`, modify `node_modules`, or use any tool not found in Step 1.**

### 3. Show auto-fix diff

```sh
git diff --stat 2>/dev/null || true
```

### 4. Find and show source files

```sh
FILES=$(git diff --name-only HEAD 2>/dev/null | grep -E '\.(ts|tsx)$')
if [ -z "$FILES" ]; then
    FILES=$(find . -maxdepth 5 \
        \( -name '*.ts' -o -name '*.tsx' \) \
        ! -path '*/node_modules/*' ! -path '*/.git/*' \
        ! -path '*/dist/*' ! -path '*/dist_*/*' ! -path '*/coverage/*')
fi
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```

### 5. Check custom coding rules

Review Steps 2–4 output. Report violations the tools **cannot** auto-fix.
One per line in this format:

```
<file>:<line>: [<severity>] <message>
```

**[高] Must fix:**
- Class component used (must use function component)
- Props not typed (all props must have explicit TypeScript types)
- User-visible string literal not wrapped in `t()` (i18n violation)
- `any` type used (forbidden)
- `interface` used for props/state (use `type`)
- camelCase variable that should be snake_case (non-function, non-component variables)
- PascalCase function that is not a React component or hook

**[中] Should fix:**
- Test file not named `test_*.ts` / `test_*.tsx` (vitest convention)
- React Bootstrap not used where a UI component is needed (bare HTML `<div>`/`<button>` etc.)
- Missing English comment at start of non-trivial code block
- Sentence-style names with prepositions (e.g. `fetchAndDisplay`)
- Hook defined outside component but missing `use` prefix
- Mutable module-level state (avoid)
- Unused import

**[低] Nice to fix:**
- `console.log` left in production code
- Long function exceeding ~50 lines

### 6. Print summary

```
==================================================
 Lint complete (TypeScript)
==================================================
 Files checked : <N>
 [高] Critical  : <N>
 [中] Important : <N>
 [低] Minor     : <N>
==================================================
```
