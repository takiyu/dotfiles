import argparse
import logging
import time
from typing import cast

import i3ipc

import swayhelper.state as _state
from swayhelper.commands import _COMMANDS
from swayhelper.ipc import SwayConn
from swayhelper.layout import _run_existing_layouts
from swayhelper.state import (BindingEvent, WindowEvent, _daemon_move_ids,
                              _IpcHandler)
from swayhelper.tree_utils import _parse_nop_commands
from swayhelper.window_ops import _swap_moved_window, _swap_new_window


# -----------------------------------------------------------------------------
# ----------------------------- Event Dispatchers -----------------------------
# -----------------------------------------------------------------------------
def on_binding(i3: SwayConn, event: BindingEvent) -> None:
    # Dispatch 'nop' binding events to registered command handlers.
    commands = _parse_nop_commands(event)
    if not commands:
        return
    try:
        i3.start_buffering()
        for argv in commands:
            handler = _COMMANDS.get(argv[0])
            if handler:
                handler(i3, event, *argv[1:])
        i3.flush()
    except Exception:
        i3.discard()
        logging.exception('binding error: %s', event.binding.command)


def on_window(i3: SwayConn, event: WindowEvent) -> None:
    # Re-tile existing workspaces after any relevant window event.
    try:
        if event.change == 'move':
            # Suppress WINDOW_MOVE events from daemon-initiated rebalancing.
            # TTL prevents a sway-dropped event from permanently blocking
            # future user moves on the same container.
            container = event.container
            expiry = _daemon_move_ids.pop(container.id, None)
            if expiry is not None and time.monotonic() < expiry:
                return
        elif event.change == 'close':
            # Remove stale entry so closed containers don't linger.
            closed: object = event.container
            _daemon_move_ids.pop(closed.id, None)
        i3.start_buffering()
        if event.change == 'new':
            # Swap the new window before its sway-assigned predecessor so it
            # appears before the focused window; done before reflow so the
            # layout engine settles on the intended final order in one pass.
            container: object = event.container
            _swap_new_window(i3, container.id)
        elif event.change == 'move':
            # User-initiated move: swap the moved window before the
            # previously-active window on the destination workspace.
            container = event.container
            _swap_moved_window(i3, container.id)
        _run_existing_layouts(i3)
        i3.flush()
    except Exception:
        i3.discard()
        logging.exception('window event error (%s)', event.change)


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
def main() -> None:
    parser = argparse.ArgumentParser(description=(
        'Sway IPC tiling daemon (swaymonad replacement).'))
    parser.add_argument(
        '--default-layout',
        default=_state.DEFAULT_LAYOUT.value,
        choices=list(_state._LAYOUT_BY_NAME),
        help='Layout for workspaces with no explicit setting.')
    parser.add_argument('--verbose', '-v', action='count',
                        help='Enable debug logging '
                        '(repeat for more verbose output).')
    parser.add_argument('--log-file',
                        help='Write log to file instead of stderr.')
    args = parser.parse_args()
    _state.DEFAULT_LAYOUT = _state._LAYOUT_BY_NAME.get(
        args.default_layout, _state.LayoutKind.TALL)

    level = logging.DEBUG if args.verbose else logging.WARNING
    fmt = '%(asctime)s %(levelname)-8s [%(filename)s:%(lineno)d] %(message)s'
    logging.basicConfig(level=level,
                        filename=args.log_file,
                        format=fmt)

    i3 = SwayConn()
    i3.on(i3ipc.Event.BINDING, cast(_IpcHandler, on_binding))
    i3.on(i3ipc.Event.WINDOW_NEW, cast(_IpcHandler, on_window))
    i3.on(i3ipc.Event.WINDOW_CLOSE, cast(_IpcHandler, on_window))
    i3.on(i3ipc.Event.WINDOW_MOVE, cast(_IpcHandler, on_window))
    i3.main()


if __name__ == '__main__':
    main()

# Backward compat re-exports for tests still importing from daemon
from swayhelper.layout import _reflow_ncol  # noqa: E402,F401
from swayhelper.layout import _run_layout  # noqa: E402,F401
from swayhelper.layout import _run_ncol_layout  # noqa: E402,F401
from swayhelper.layout import _run_nop_layout  # noqa: E402,F401
from swayhelper.state import LayoutKind  # noqa: E402,F401
from swayhelper.state import Transform  # noqa: E402,F401
from swayhelper.state import WorkspaceState  # noqa: E402,F401
from swayhelper.state import _ws_states  # noqa: E402,F401
from swayhelper.window_ops import _get_master_window  # noqa: E402,F401
from swayhelper.window_ops import _get_resize_target  # noqa: E402,F401

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
