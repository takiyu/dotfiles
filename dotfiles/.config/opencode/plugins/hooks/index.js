// Plugin that reads hooks.json and runs bash scripts on opencode events.
// Mirrors copilot's postToolUse and sessionEnd hook mechanism.
import { readFileSync } from 'fs';
import { spawn } from 'child_process';
import { homedir } from 'os';
import { resolve } from 'path';


// Resolve hooks.json from the config directory
const CONFIG_DIR = resolve(homedir(), '.config', 'opencode');
const HOOKS_JSON = resolve(CONFIG_DIR, 'hooks.json');


// Load hooks config; return empty hooks on error
function load_hooks() {
  try {
    const raw = readFileSync(HOOKS_JSON, 'utf8');
    return JSON.parse(raw).hooks || {};
  } catch (err) {
    console.error('[HooksPlugin] Failed to load hooks.json:', err);
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
      stdio: ['pipe', 'pipe', 'pipe'],
      timeout: (timeout_sec || 30) * 1000,
      env: process.env,
    });
    proc.stdin.write(JSON.stringify(input_json));
    proc.stdin.end();

    let stderr_data = '';
    proc.stderr.on('data', (data) => { stderr_data += data; });

    proc.on('close', (code) => {
      if (code !== 0) {
        console.error(
          `[HooksPlugin] Script exited with code ${code}:` +
          ` ${script_path} stderr:`, stderr_data
        );
      }
      resolve_promise();
    });
    proc.on('error', (err) => {
      console.error(`[HooksPlugin] Failed to spawn script:`, err);
      resolve_promise();
    });
  });
}


export const HooksPlugin = async ({ directory }) => {
  const hooks = load_hooks();

  // Run all scripts for a given event
  async function run_hooks_for(event_name, payload) {
    const entries = hooks[event_name] || [];
    if (!entries.length) {
      console.log(`[HooksPlugin] No hooks configured for ${event_name}`);
      return;
    }
    for (const entry of entries) {
      if (entry.bash) {
        await run_script(entry.bash, payload, entry.timeoutSec);
      }
    }
  }

  async function handle_session_idle(event) {
    const sessionId = event?.sessionID || event?.properties?.sessionID || '';
    const cwd = event?.cwd || directory;
    console.log(`[HooksPlugin] session.idle: sessionId=${sessionId}`);
    await run_hooks_for('session.idle', {
      cwd: cwd,
      sessionId: sessionId,
    });
  }

  async function handle_permission_asked(event) {
    const data = event?.data || event;
    const permId = data?.id || '';
    const permType = data?.type || '';
    const title = data?.title || '';
    const patterns = data?.patterns || data?.pattern || [];
    console.log(
      `[HooksPlugin] permission.asked: id=${permId} type=${permType}`
    );
    await run_hooks_for('permission.ask', {
      permissionId: permId,
      permissionType: permType,
      title: title,
      patterns: patterns,
      cwd: directory,
    });
  }

  return {
    // Listen to bus events (session.idle, permission.asked, etc)
    event: async ({ event }) => {
      const eventName = event?.type || '';
      if (!eventName) return;

      switch (eventName) {
        case 'session.idle':
          await handle_session_idle(event);
          break;
        case 'permission.asked':
          await handle_permission_asked(event);
          break;
        default:
          // ignore other events
          break;
      }
    },

    // postToolUse equivalent: run after each tool execution
    'tool.execute.after': async (input, _output) => {
      await run_hooks_for('tool.execute.after', {
        toolName: input.tool,
        cwd: directory,
        toolArgs: input.args || {},
      });
    },

    // Permission-ask trigger (if called directly)
    'permission.ask': async (input, _output) => {
      console.log('[HooksPlugin] permission.ask trigger:', input);
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

export default HooksPlugin;
