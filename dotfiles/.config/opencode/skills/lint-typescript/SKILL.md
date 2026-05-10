---
name: lint-typescript
description: Run TypeScript/React lint tools (make lint → npm run lint → npx eslint --fix), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash
---

# ------------------------------------------------------------------------------
# --------------------------------- Lint TypeScript ----------------------------
# ------------------------------------------------------------------------------

# Run formatters, linters, and custom coding rule checks for TypeScript/React projects.


# ------------------------------------------------------------------------------
# ------------------------------- Step 1: Detect -------------------------------
# ------------------------------------------------------------------------------

# Detect available tools and project configuration.

```sh
# Detect available tools
echo "=== tools available ==="
for t in make npm npx tsc; do
    command -v "$t" >/dev/null 2>&1 \
        && echo "$t: $(command -v $t)" || echo "$t: not found"
done

# Check ESLint availability via npx
echo "=== npx eslint ==="
npx eslint --version 2>/dev/null || echo "npx eslint: not available"

# Check for Makefile lint target
echo "=== Makefile lint target ==="
grep -c '^lint:' Makefile 2>/dev/null \
    && echo "Makefile has lint target" || echo "no Makefile lint"

# Check for package.json lint script
echo "=== package.json lint script ==="
node -e "const p=require('./package.json'); console.log(p.scripts&&p.scripts.lint ? 'lint: '+p.scripts.lint : 'no lint script')" 2>/dev/null || echo "no package.json"
```


# ------------------------------------------------------------------------------
# ------------------------------- Step 2: Run ----------------------------------
# ------------------------------------------------------------------------------

# Choose ONE branch based on Step 1 detection results.

**Branch A** — Makefile has `lint:` target:
```sh
# Run make lint target
make lint 2>&1
```

**Branch B** — package.json has `lint` script:
```sh
# Run npm lint script
npm run lint 2>&1
```

**Branch C** — fallback to direct ESLint:
```sh
# Run ESLint with auto-fix and TypeScript compiler check
npx eslint --fix . 2>&1 || true
command -v tsc >/dev/null 2>&1 \
    && tsc --noEmit 2>&1 || true
```

**Do NOT run `npm install`, modify `node_modules`, or use any tool not found in Step 1.**


# ------------------------------------------------------------------------------
# --------------------------- Step 3: Find Files -------------------------------
# ------------------------------------------------------------------------------

# Discover and display relevant source files for review.

```sh
# Find relevant TypeScript source files for review
FILES=$(find . -maxdepth 5 \
    \( -name '*.ts' -o -name '*.tsx' \) \
    ! -path '*/node_modules/*' ! -path '*/.git/*' \
    ! -path '*/dist/*' ! -path '*/dist_*/*' ! -path '*/coverage/*')
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```


# ------------------------------------------------------------------------------
# ---------------------- Step 4: Custom Rules Check ----------------------------
# ------------------------------------------------------------------------------

# Review Steps 2–3 output. Report violations the tools **cannot** auto-fix.
# One per line in this format:

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
- Named section header (`# ---` x3 with centered name) not preceded by
  exactly 2 blank lines, OR followed by any blank lines — must have 2
  empty lines before and 0 after (code starts immediately)
- Separator block (`# ---` x3 with no name, e.g. interface/impl divider or
  EOF terminator) not preceded by exactly 2 blank lines, OR not followed
  by exactly 1 blank line — must have 2 empty lines before and 1 after
- Section delimiter line length or alignment wrong: every line must be
  exactly 80 chars with no trailing whitespace. Lines 1 and 3 must be
  `# ` + 78 `-` (or `// ` + 78 `-`). Line 2 must center the section
  name with left and right `-` counts equal or differing by exactly 1
  (right side gets the extra `-` when total is odd, never a trailing
  space)
- File does not end with exactly 3 lines of `# ------------------------------------------------------------------------------`
  (each 80 chars) with no trailing newline after the last line

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


# ------------------------------------------------------------------------------
# --------------------------- Step 5: Print Summary ----------------------------
# ------------------------------------------------------------------------------

# Print final summary of lint results.

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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
