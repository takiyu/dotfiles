// Rich notification plugin with project details
import path from 'path';
import { spawn } from 'child_process';
import { fileURLToPath } from 'url';

// V2 plugin definition: { id, setup } shape validated by the plugin loader.

const plugin_dir = path.dirname(fileURLToPath(import.meta.url));

const APP_NAME = 'opencode';
const FALLBACK_TITLE = 'OpenCode';
const ICON = 'dialog-information';
const ICON_COMPLETE = 'emblem-default';
const ICON_ERROR = 'dialog-error';
const ICON_PERMISSION = 'dialog-question';
const URGENCY_NORMAL = 'normal';
const URGENCY_CRITICAL = 'critical';
const ERROR_EXPIRE_MS = 60000;

const SOUND_DIR = path.join(plugin_dir, 'sounds');
const SOUND_COMPLETE = path.join(SOUND_DIR, 'complete.oga');
const SOUND_ERROR = path.join(SOUND_DIR, 'dialog-error.oga');
const SOUND_PERMISSION = path.join(SOUND_DIR, 'bell.oga');


// -----------------------------------------------------------------------------
// -------------------------------- Interfaces ---------------------------------
// -----------------------------------------------------------------------------
// Send desktop notifications and sounds for session and permission events.
const definition = {
  id: 'notification',
  setup(ctx) {
    const project = ctx.location.project || {};
    let dir_name = FALLBACK_TITLE;
    if (ctx.location.directory) {
      dir_name = path.basename(ctx.location.directory);
    }
    const title = project.name || dir_name || FALLBACK_TITLE;
    const description = project.description || '';
    const controller = new AbortController();

    // Subscribe to server events for the whole plugin lifetime.
    void (async () => {
      const options = { signal: controller.signal };
      for await (const event of ctx.event.subscribe(options)) {
        handleEvent(event, title, description);
      }
    })();

    return () => controller.abort();
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
// Notify the user for supported event types.
function handleEvent(event, title, description) {
  if (event.type === 'session.execution.succeeded') {
    const status = event.message || 'Session completed';
    sendNotification(title, status, URGENCY_NORMAL, description,
      ICON_COMPLETE, SOUND_COMPLETE);
  }
  if (event.type === 'session.execution.failed') {
    const status = event.message || event.error || 'An error occurred';
    sendNotification(title, status, URGENCY_NORMAL, description,
      ICON_ERROR, SOUND_ERROR, ERROR_EXPIRE_MS, true);
  }
  if (event.type === 'permission.asked') {
    const status = event.message || 'Task paused — permission requested';
    sendNotification(title, status, URGENCY_NORMAL, description,
      ICON_PERMISSION, SOUND_PERMISSION);
  }
}


// Send a desktop notification via `notify-send` and play a sound file.
function sendNotification(title, status, urgency, description,
                          icon_override, sound_file, expire_ms = null,
                          transient = false) {
  let icon = ICON;
  if (urgency === URGENCY_CRITICAL) icon = ICON_ERROR;
  if (icon_override) icon = icon_override;
  const args = ['-a', APP_NAME, '-u', urgency, '-i', icon];
  if (expire_ms !== null) args.push('-t', String(expire_ms));
  if (transient) args.push('-e');
  let body = status;
  if (description) body = `${status}\n${description}`;
  spawnCommand('notify-send', args.concat([title, body]));
  if (sound_file) spawnCommand('paplay', [sound_file]);
}


// Fire a helper command and ignore availability failures.
function spawnCommand(cmd, args) {
  try {
    const child = spawn(cmd, args, { stdio: 'ignore' });
    child.on('error', () => {});
  } catch (error) {
    void error;
  }
}


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------