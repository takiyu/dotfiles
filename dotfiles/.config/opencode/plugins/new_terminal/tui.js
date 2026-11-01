import { spawn } from 'node:child_process';


// -----------------------------------------------------------------------------
// -------------------------------- Interfaces ---------------------------------
// -----------------------------------------------------------------------------
const definition = {
  id: 'new_terminal.tui',
  setup(context) {
    // Removing the keymap layer when the plugin unloads
    const removeLayer = context.keymap.layer(() => ({
      mode: 'global',
      priority: 10,
      commands: [{
        id: 'new_terminal.open',
        title: 'Open external terminal',
        group: 'Session',
        bind: 'ctrl+t',
        palette: true,
        run: () => openTerminal(context),
      }],
    }));
    return () => removeLayer?.();
  },
};


export default definition;


// -----------------------------------------------------------------------------
// ------------------------------ Implementation -------------------------------
// -----------------------------------------------------------------------------
const TERMINAL_COMMAND = 'xfce4-terminal';


// Launching a detached terminal in the session directory
function openTerminal(context) {
  const directory = sessionDirectory(context);
  if (!directory) {
    return showError(context, 'No directory for this session');
  }
  const child = spawn(
    TERMINAL_COMMAND,
    ['--disable-server'],
    { cwd: directory, detached: true, stdio: 'ignore' }
  );
  // Reporting spawn failures without crashing the TUI
  child.on('error', (error) => showError(context, error.message));
  child.unref();
}


// Resolving the current session directory from the router
function sessionDirectory(context) {
  const route = context.ui.router.current();
  if (route?.type !== 'session') return context.location?.directory;
  return context.data.session.get(route.sessionID)?.location?.directory;
}


// Showing a failure toast in the TUI
function showError(context, message) {
  context.ui.toast.show({ title: 'New terminal', message, variant: 'error' });
}


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
