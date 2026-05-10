---
name: lint-javascript
description: Run JavaScript/React lint tools (make lint → npm run lint → npx eslint --fix), then check custom coding rules. Reports violations with [高]/[中]/[低] severity labels in Japanese.
allowed-tools: bash
---

# ------------------------------------------------------------------------------
# --------------------------------- Lint JavaScript ----------------------------
# ------------------------------------------------------------------------------

# Run formatters, linters, and custom coding rule checks for JavaScript/React projects.


# ------------------------------------------------------------------------------
# ------------------------------- Step 1: Detect -------------------------------
# ------------------------------------------------------------------------------

# Detect available tools and project configuration.

```sh
# Detect available tools
echo "=== tools available ==="
for t in make npm npx; do
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
# Run ESLint directly with auto-fix
npx eslint --fix . 2>&1 || true
```

**Do NOT run `npm install`, modify `node_modules`, or use any tool not found in Step 1.**


# ------------------------------------------------------------------------------
# ---------------------------- Step 3: Show Diff -------------------------------
# ------------------------------------------------------------------------------

# Show statistics of auto-fixed changes.

```sh
# Show git diff statistics for auto-fixed changes
git diff --stat 2>/dev/null || true
```


# ------------------------------------------------------------------------------
# --------------------------- Step 4: Find Files -------------------------------
# ------------------------------------------------------------------------------

# Discover and display relevant source files for review.

```sh
# Find modified or all relevant source files for review
FILES=$(git diff --name-only HEAD -- . 2>/dev/null | grep -E '\.(js|jsx)$')
if [ -z "$FILES" ]; then
    FILES=$(find . -maxdepth 5 \
        \( -name '*.js' -o -name '*.jsx' \) \
        ! -path '*/node_modules/*' ! -path '*/.git/*' \
        ! -path '*/dist/*' ! -path '*/dist_*/*' ! -path '*/coverage/*')
fi
for f in $FILES; do echo "====== $f ======"; cat -n "$f"; echo; done
```


# ------------------------------------------------------------------------------
# ---------------------- Step 5: Custom Rules Check ----------------------------
# ------------------------------------------------------------------------------

# Review Steps 2–4 output. Report violations the tools **cannot** auto-fix.
# One per line in this format:

```
<file>:<line>: [<severity>] <message>
```

**[高] Must fix:**
- Class component used (must use function component)
- User-visible string literal not wrapped in `t()` (i18n violation)
- Mixed module systems (`require()` and `import/export` in same file)
- camelCase variable that should be snake_case (non-function, non-component variables)
- PascalCase function that is not a React component or hook
- Section delimiter (`# ---` or `// ---`) line length or alignment wrong:
  every line must be exactly 80 chars. Lines 1 and 3 must be `# ` + 78 `-`
  (or `// ` + 78 `-`). Line 2 must center the section name with left and
  right `-` counts equal or differing by exactly 1

**[中] Should fix:**
- React Bootstrap not used where a UI component is needed (bare HTML `<div>`/`<button>` etc.)
- Missing English comment at start of non-trivial code block
- Sentence-style names with prepositions (e.g. `fetchAndDisplay`)
- Hook defined outside component but missing `use` prefix
- Mutable module-level state (avoid)
- Unused import / require

**[低] Nice to fix:**
- `console.log` left in production code
- Long function exceeding ~50 lines


# ------------------------------------------------------------------------------
# --------------------------- Step 6: Print Summary ----------------------------
# ------------------------------------------------------------------------------

# Print final summary of lint results.

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


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
