// Plugin that reads hooks.json and runs bash scripts on opencode events.
// Mirrors copilot's postToolUse and sessionEnd hook mechanism.
import { createRequire } from 'module';
import { readFileSync } from 'fs';
import { spawn } from 'child_process';
import { homedir } from 'os';
import { resolve } from 'path';


// Resolve hooks.json from the config directory (same dir as this plugin)
const CONFIG_DIR = resolve(homedir(), '.config', 'opencode');
const HOOKS_JSON = resolve(CONFIG_DIR, 'hooks.json');


// Load hooks config; return empty hooks on error
function load_hooks() {
  try {
    const raw = readFileSync(HOOKS_JSON, 'utf8');
    return JSON.parse(raw).hooks || {};
  } catch (_) {
    return {};
  }
}


// Expand $HOME in bash command path
function expand_env(str) {
  return str.replace(/\$HOME/g, homedir());
}


// Run a bash script, piping inputJson to its stdin; resolve when done
function run_script(script_path, input_json, timeout_sec) {
  return new Promise((resolve_promise) => {
    const script = expand_env(script_path);
    const proc = spawn('bash', [script], {
      stdio: ['pipe', 'inherit', 'inherit'],
      timeout: (timeout_sec || 30) * 1000,
    });
    proc.stdin.write(JSON.stringify(input_json));
    proc.stdin.end();
    proc.on('close', resolve_promise);
    proc.on('error', resolve_promise);
  });
}


export const HooksPlugin = async ({ directory }) => {
  const hooks = load_hooks();

  // Run all scripts for a given event
  async function run_hooks_for(event_name, payload) {
    const entries = hooks[event_name] || [];
    for (const entry of entries) {
      if (entry.bash) {
        await run_script(entry.bash, payload, entry.timeoutSec);
      }
    }
  }

  return {
    // postToolUse equivalent: run after each tool execution
    'tool.execute.after': async (input, _output) => {
      await run_hooks_for('tool.execute.after', {
        toolName: input.tool,
        cwd: directory,
        toolArgs: input.args || {},
      });
    },

    // sessionEnd equivalent: run when session becomes idle
    'session.idle': async (event) => {
      await run_hooks_for('session.idle', {
        cwd: directory,
        sessionId: event?.properties?.sessionID || '',
      });
    },

    // Permission-ask equivalent: notify user before permission dialog appears
    'permission.ask': async (input, _output) => {
      await run_hooks_for('permission.ask', {
        permissionId: input.id,
        permissionType: input.type,
        title: input.title,
        patterns: input.pattern || [],
        cwd: directory,
      });
    },
  };
};
