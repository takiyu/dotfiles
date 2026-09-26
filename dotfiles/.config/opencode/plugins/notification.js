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

    // Subscribe to server events and notify only for own sessions.
    void (async () => {
      const options = { signal: controller.signal };
      for await (const event of ctx.event.subscribe(options)) {
        const request = notificationFor(event);
        if (!request) continue;
        const owner = await sessionDirectory(ctx, (event.data || {}).sessionID);
        if (owner !== ctx.location.directory) continue;
        sendNotification(title, request.status, URGENCY_NORMAL, description,
          request.icon, request.sound, request.expire_ms, request.transient);
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
// Map a server event to a notification request, or null to skip.
function notificationFor(event) {
  const data = event.data || {};
  if (event.type === 'session.execution.succeeded') {
    return { status: data.message || 'Session completed',
      icon: ICON_COMPLETE, sound: SOUND_COMPLETE };
  }
  if (event.type === 'session.execution.failed') {
    return { status: data.message || data.error || 'An error occurred',
      icon: ICON_ERROR, sound: SOUND_ERROR,
      expire_ms: ERROR_EXPIRE_MS, transient: true };
  }
  if (event.type === 'permission.asked') {
    return { status: data.message || 'Task paused — permission requested',
      icon: ICON_PERMISSION, sound: SOUND_PERMISSION };
  }
  return null;
}


// Resolve the directory that owns the given session.
async function sessionDirectory(ctx, session_id) {
  if (!session_id) return null;
  try {
    const session = await ctx.session.get({ sessionID: session_id });
    return (session && session.location && session.location.directory) || null;
  } catch (error) {
    void error;
    return null;
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