// Plugin that bridges opencode hooks to lint-on-write.sh and reinforces
// coding-style rules in every system prompt.
import path from 'path';


// -----------------------------------------------------------------------------
// ----------------------------- Formatting rules ------------------------------
// -----------------------------------------------------------------------------
const FORMATTING_RULES = `

# STRICT FORMATTING RULES (always enforce)

## Section Delimiters (Python: 79 chars, TypeScript/Shell: 80 chars)
- Named header (center text, surround with spaces and dashes):
  Python:   # <surround name with dashes so total line is 79 chars>
  Example:  # ------------------------------- Section Name --------------------------------
- Pure separator (no text): Python 79 dashes, TS/Shell 80 dashes.
- Named header MUST be flanked by two separator lines.
- NO trailing spaces on delimiter lines.

## Code Style Quick Reference
- Python: 79 chars/line, snake_case, single quotes, type hints mandatory.
- TypeScript: 100 chars/line, 2-space indent, PascalCase classes, camelCase functions.
- NEVER omit type annotations; use Optional[X] instead of X | None.
- Use dict()/list() for empty containers, not {}/[].
- Extract pure functions; avoid class methods / global mutable state.
- Keep comments in English.
`;

const LINT_SCRIPT = path.join(
  process.env.HOME,
  '.config/opencode/hooks/lint-on-write.sh'
);

// -----------------------------------------------------------------------------
// ------------------------------ Plugin factory -------------------------------
// -----------------------------------------------------------------------------
export default async function hooksPlugin({ $ }) {
  return {
    // Inject formatting rules into every system prompt so the model
    // sees them before it writes any code.
    'experimental.chat.system.transform': async (_input, output) => {
      output.system = output.system || [];
      // Prepend our rules so they appear early in the prompt.
      output.system.unshift(FORMATTING_RULES);
    },

    // Run lint checks after edit/write tool executions.
    event: async ({ event }) => {
      if (event.type === 'tool.execute.after') {
        const toolName = event.tool?.toLowerCase() || '';
        if (toolName !== 'edit' && toolName !== 'write') {
          return;
        }

        const payload = JSON.stringify({
          toolName: event.tool,
          toolArgs: JSON.stringify({
            path: (event.args?.path || event.args?.filePath || '')
          }),
          cwd: event.cwd || process.cwd(),
        });

        try {
          await $`echo ${payload} | bash ${LINT_SCRIPT}`.quiet();
        } catch (e) {
          if (e.stderr) {
            console.error(e.stderr);
          }
        }
      }
    },
  };
}

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
