// Plugin that reads hooks.json and runs bash scripts on opencode events.
// Mirrors copilot's postToolUse and sessionEnd hook mechanism.
import { createRequire } from "module";
import { readFileSync } from "fs";
import { spawn } from "child_process";
import { resolve } from "path";
import { homedir } from "os";

// Resolve hooks.json from the config directory (same dir as this plugin)
const CONFIG_DIR = resolve(homedir(), ".config", "opencode");
const HOOKS_JSON = resolve(CONFIG_DIR, "hooks.json");

// Load hooks config; return empty hooks on error
function loadHooks() {
  try {
    const raw = readFileSync(HOOKS_JSON, "utf8");
    return JSON.parse(raw).hooks || {};
  } catch (_) {
    return {};
  }
}

// Expand $HOME in bash command path
function expandEnv(str) {
  return str.replace(/\$HOME/g, homedir());
}

// Run a bash script, piping inputJson to its stdin; resolve when done
function runScript(scriptPath, inputJson, timeoutSec) {
  return new Promise((resolvePromise) => {
    const script = expandEnv(scriptPath);
    const proc = spawn("bash", [script], {
      stdio: ["pipe", "inherit", "inherit"],
      timeout: (timeoutSec || 30) * 1000,
    });
    proc.stdin.write(JSON.stringify(inputJson));
    proc.stdin.end();
    proc.on("close", resolvePromise);
    proc.on("error", resolvePromise);
  });
}

export const HooksPlugin = async ({ directory }) => {
  const hooks = loadHooks();

  // Run all scripts for a given event
  async function runHooksFor(eventName, payload) {
    const entries = hooks[eventName] || [];
    for (const entry of entries) {
      if (entry.bash) {
        await runScript(entry.bash, payload, entry.timeoutSec);
      }
    }
  }

  return {
    // postToolUse equivalent: run after each tool execution
    "tool.execute.after": async (input, _output) => {
      await runHooksFor("tool.execute.after", {
        toolName: input.tool,
        cwd: directory,
        toolArgs: input.args || {},
      });
    },

    // sessionEnd equivalent: run when session becomes idle
    "session.idle": async (event) => {
      await runHooksFor("session.idle", {
        cwd: directory,
        sessionId: event?.properties?.sessionID || "",
      });
    },
  };
};
