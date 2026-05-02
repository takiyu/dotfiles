import logging

from swayhelper.constants import (LAYOUT_BY_NAME, BindingEvent, CmdHandler,
                                  LayoutKind, Transform)
from swayhelper.ipc import SwayConn
from swayhelper.layout import (_apply_reflectx, _apply_reflecty,
                               _apply_transpose, _run_layout)
from swayhelper.tree_utils import (_get_focused_window, _get_focused_workspace,
                                   _get_ws_state, _refetch)
from swayhelper.window_ops import (_focus_window, _get_master_window,
                                   _get_resize_target, _swap_window)


# -----------------------------------------------------------------------------
# ----------------------------- Command Handlers ------------------------------
# -----------------------------------------------------------------------------
def _cmd_promote_window(i3: SwayConn,
                        event: BindingEvent, *args: str) -> None:
    # Swap the focused window with the primary master window.
    ws = _get_focused_workspace(i3)
    focused = _get_focused_window(i3)
    if ws is None or focused is None:
        return
    state = _get_ws_state(ws.id)
    master = _get_master_window(ws, state)
    if master is None or focused.id == master.id:
        return
    focused.command(f'swap container with con_id {master.id}')
    focused.command('focus')


def _cmd_focus_master(i3: SwayConn,
                      event: BindingEvent, *args: str) -> None:
    # Move focus to the primary master window.
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    master = _get_master_window(ws, state)
    if master:
        master.command('focus')


def _cmd_resize_master(i3: SwayConn,
                       event: BindingEvent, *args: str) -> None:
    # Resize the master window. Extra args forwarded to sway resize.
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    master = _get_resize_target(ws, state)
    if master:
        master.command('resize ' + ' '.join(args))


def _cmd_focus_next_window(i3: SwayConn,
                           event: BindingEvent, *args: str) -> None:
    _focus_window(i3, offset=+1)


def _cmd_focus_prev_window(i3: SwayConn,
                           event: BindingEvent, *args: str) -> None:
    _focus_window(i3, offset=-1)


def _cmd_swap_next_window(i3: SwayConn,
                          event: BindingEvent, *args: str) -> None:
    _swap_window(i3, offset=+1)


def _cmd_swap_prev_window(i3: SwayConn,
                          event: BindingEvent, *args: str) -> None:
    _swap_window(i3, offset=-1)


def _cmd_set_layout(i3: SwayConn,
                    event: BindingEvent, *args: str) -> None:
    # Change the current workspace to a named layout and re-tile.
    if not args:
        return
    kind = LAYOUT_BY_NAME.get(args[0])
    if kind is None:
        logging.warning('Unknown layout: %s', args[0])
        return
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.kind = kind
    _run_layout(i3, ws.id)


def _cmd_increment_masters(i3: SwayConn,
                           event: BindingEvent, *args: str) -> None:
    # Add one master slot to the current workspace and re-tile.
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters += 1
    _run_layout(i3, ws.id)


def _cmd_decrement_masters(i3: SwayConn,
                           event: BindingEvent, *args: str) -> None:
    # Remove one master slot (minimum 1) and re-tile.
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters = max(1, state.n_masters - 1)
    _run_layout(i3, ws.id)


def _cmd_toggle_transform(i3: SwayConn, event: BindingEvent,
                          transform: Transform) -> None:
    # Toggle a geometric transformation and re-tile the workspace.
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    if transform in state.transforms:
        state.transforms.discard(transform)
    else:
        state.transforms.add(transform)
    # Physically apply the flip/transpose to the container tree
    ws_fresh = _refetch(i3, ws)
    if ws_fresh is not None:
        if transform == Transform.TRANSPOSE:
            _apply_transpose(i3, ws_fresh)
        elif transform == Transform.REFLECTX:
            _apply_reflectx(i3, ws_fresh)
        elif transform == Transform.REFLECTY:
            _apply_reflecty(i3, ws_fresh)
    _run_layout(i3, ws.id)


def _cmd_transpose(i3: SwayConn,
                   event: BindingEvent, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.TRANSPOSE)


def _cmd_reflectx(i3: SwayConn,
                  event: BindingEvent, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.REFLECTX)


def _cmd_reflecty(i3: SwayConn,
                  event: BindingEvent, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.REFLECTY)


def _cmd_move(i3: SwayConn,
              event: BindingEvent, *args: str) -> None:
    # Move the focused window in a direction with layout awareness.
    if not args:
        return
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    direction = args[0]
    state = _get_ws_state(ws.id)
    if state.kind == LayoutKind.NOP:
        i3.command(f'move {direction}')
    else:
        focused = _get_focused_window(i3)
        i3.command(f'focus {direction}')
        neighbor = _get_focused_window(i3)
        if focused and neighbor and focused.id != neighbor.id:
            focused.command(f'swap container with con_id {neighbor.id}')
            focused.command('focus')


def _cmd_fullscreen(i3: SwayConn,
                    event: BindingEvent, *args: str) -> None:
    # Toggle fullscreen on the focused window.
    i3.command('fullscreen')


# Map nop command names to their handler functions
_COMMANDS: dict[str, CmdHandler] = {
    'promote_window':        _cmd_promote_window,
    'focus_master':          _cmd_focus_master,
    'resize_master':         _cmd_resize_master,
    'focus_next_window':     _cmd_focus_next_window,
    'focus_prev_window':     _cmd_focus_prev_window,
    'swap_with_next_window': _cmd_swap_next_window,
    'swap_with_prev_window': _cmd_swap_prev_window,
    'set_layout':            _cmd_set_layout,
    'increment_masters':     _cmd_increment_masters,
    'decrement_masters':     _cmd_decrement_masters,
    'transpose':             _cmd_transpose,
    'reflectx':              _cmd_reflectx,
    'reflecty':              _cmd_reflecty,
    'move':                  _cmd_move,
    'fullscreen':            _cmd_fullscreen,
}


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
