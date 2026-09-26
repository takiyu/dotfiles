import path from 'path';
import { spawn } from 'child_process';

// V2 plugin definition: { id, setup } shape validated by the plugin loader.

const LINT_SCRIPT = path.join(
  process.env.HOME,
  '.config/opencode/hooks/lint-on-write.sh'
);
const REQUEST_KINDS = ['context', 'compaction', 'generate', 'title'];
const LINT_TOOL_NAMES = ['edit', 'write'];
const RULES_PROMPT = `
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
`;


// -----------------------------------------------------------------------------
// -------------------------------- Interfaces ---------------------------------
// -----------------------------------------------------------------------------
// Inject formatting rules and lint files after write/edit tool calls.
const definition = {
  id: 'hooks',
  async setup(ctx) {
    // Prepend shared formatting rules to every model request prompt.
    for (const kind of REQUEST_KINDS) {
      await ctx.session.hook(kind, (event) => {
        event.system = event.system || [];
        event.system.unshift({ type: 'text', text: RULES_PROMPT });
      });
    }

    // Run lint checks after edit/write tool executions.
    await ctx.tool.hook('execute.after', async (event) => {
      const tool_name = String(event.tool || '').toLowerCase();
      if (!LINT_TOOL_NAMES.includes(tool_name)) return;
      await runLint(buildPayload(event, ctx.location.directory));
    });
  }
};


// Dual default/named export keeps CJS and ESM plugin loaders working.
export default definition;
export { definition as plugin };

// CJS interop fallback for the plugin loader module evaluation.
if (typeof module !== 'undefined') {
  module.exports = definition;
  module.exports.default = definition;
}


// -----------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// -----------------------------------------------------------------------------
// Build the JSON payload consumed by lint-on-write.sh on stdin.
function buildPayload(event, fallback_cwd) {
  const input = event.input || {};
  const file_path = input.path || input.filePath || '';
  return JSON.stringify({
    toolName: event.tool,
    toolArgs: JSON.stringify({ path: file_path }),
    cwd: event.cwd || fallback_cwd
  });
}


// Pipe the payload into lint-on-write.sh; failures are non-fatal.
async function runLint(payload) {
  try {
    await new Promise((resolve, reject) => {
      const child = spawn('bash', [LINT_SCRIPT], {
        stdio: ['pipe', 'ignore', 'inherit']
      });
      child.on('error', reject);
      child.on('close', resolve);
      child.stdin.end(payload);
    });
  } catch (error) {
    void error;
  }
}


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------