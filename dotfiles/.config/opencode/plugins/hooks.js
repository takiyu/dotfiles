#!/usr/bin/env node
import path from 'path';

const LINT_SCRIPT = path.join(
  process.env.HOME,
  '.config/opencode/hooks/lint-on-write.sh'
);

export default async function hooksPlugin({ $ }) {
  return {
    // Inject formatting rules into every system prompt.
    'experimental.chat.system.transform': async (_input, output) => {
      output.system = output.system || [];
      output.system.unshift(`
# STRICT FORMATTING RULES

## Section Delimiters (Python: 79 chars, TypeScript/Shell: 80 chars)
- Named header: center text, total line must be exactly 79/80 chars.
- Pure separator: 79/80 dashes.
- Named header MUST have 2 blank lines before and 0 after.
- Separator block MUST have 2 blank lines before and 1 after.
- NO trailing spaces.

## Code Style Quick Reference
- Python: 79 chars/line, snake_case, single quotes, type hints mandatory.
- TypeScript: 100 chars/line, 2-space indent, PascalCase classes, camelCase functions.
- NEVER omit type annotations; use Optional[X] instead of X | None.
- Use dict()/list() for empty containers.
- Extract pure functions; avoid class methods / global mutable state.
- Keep comments in English.
`);
    },

    // Run lint checks after edit/write tool executions.
    event: async ({ event }) => {
      if (event.type !== 'tool.execute.after') {
        return;
      }
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
    },
  };
}
