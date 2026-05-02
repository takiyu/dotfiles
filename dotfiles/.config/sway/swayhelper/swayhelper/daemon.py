# Sway IPC tiling daemon.
# Self-contained replacement for the swaymonad dependency.
# Listens for sway 'nop' binding events and manages auto-tiling layouts.
# Invoke once at startup:  exec_always ~/.config/sway/sway_helper_daemon.py
import argparse
import enum
import logging
import math
import shlex
import time
from typing import Callable, Optional, cast

import i3ipc
import i3ipc.events

from swayhelper.constants import MOVE_MARK

# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
DEFAULT_LAYOUT = 'tall'  # Default for landscape; portrait auto-uses 'stack'
# Workspace name prefixes used by helper.py for temporary rename operations.
# These workspaces must be skipped during layout to avoid tiling zombies.
_TEMP_WS_PREFIXES = ('__ws_', '__swh_tmp_')

# Maximum reflow iterations per window; scales the convergence guard in
# _run_ncol_layout to handle large workspaces without false timeouts.
MAX_REFLOW_ITERS_PER_WIN = 3

# Time-to-live for daemon-move suppression entries (seconds).
# Generous bound; sway events typically arrive within milliseconds.
_MOVE_ID_TTL = 2.0


# -----------------------------------------------------------------------------
# ----------------------------------- Types -----------------------------------
# -----------------------------------------------------------------------------
class LayoutKind(enum.Enum):
    TALL = 'tall'
    STACK = 'stack'
    THREE_COL = '3_col'
    NOP = 'nop'


class Transform(enum.Enum):
    TRANSPOSE = 'TRANSPOSE'
    REFLECTX = 'REFLECTX'
    REFLECTY = 'REFLECTY'


# Number of tiling columns per layout kind (NOP has no column-based tiling)
_LAYOUT_COLS: dict[LayoutKind, int] = {
    LayoutKind.TALL: 2,
    LayoutKind.STACK: 1,
    LayoutKind.THREE_COL: 3,
}

# String-to-enum map for use with the 'set_layout' command
_LAYOUT_BY_NAME: dict[str, LayoutKind] = {k.value: k for k in LayoutKind}

# i3ipc type aliases; Con attrs are set dynamically via setattr, so alias to
# Any to avoid false positives from pyright when stubs are not present.
Con = object
WindowEvent = i3ipc.events.WindowEvent
BindingEvent = i3ipc.events.BindingEvent

# Callable type for nop command handlers (i3, event, *args)
_CmdHandler = Callable[..., None]

# Callable type matching i3ipc.Connection.on() handler signature
_IpcHandler = Callable[
    [i3ipc.Connection, i3ipc.events.IpcBaseEvent], None]

# -----------------------------------------------------------------------------
# ------------------------------ Workspace State ------------------------------
# -----------------------------------------------------------------------------
class WorkspaceState:
    # Tiling layout state for a single sway workspace.

    def __init__(self, ws_id: int,
                 kind: LayoutKind = LayoutKind.TALL,
                 n_masters: int = 1,
                 transforms: Optional[set[Transform]] = None):
        self.ws_id = ws_id
        self.kind = kind
        self.n_masters = n_masters
        self.transforms: set[Transform] = (
            set() if transforms is None else set(transforms))

    @property
    def n_columns(self) -> int:
        # Number of tiling columns for this layout kind.
        return _LAYOUT_COLS.get(self.kind, 1)


# Per-workspace layout states, keyed by workspace con_id
_ws_states: dict[int, WorkspaceState] = dict()

# Pending daemon-initiated moves: container_id -> expiry time (monotonic).
# The resulting WINDOW_MOVE event is suppressed to prevent infinite reflow.
# Entries expire after _MOVE_ID_TTL seconds so a sway-dropped event never
# permanently suppresses future user moves on the same container.
_daemon_move_ids: dict[int, float] = dict()


# -----------------------------------------------------------------------------
# ------------------------------ IPC Connection -------------------------------
# -----------------------------------------------------------------------------
class SwayConn(i3ipc.Connection):
    # Sway IPC connection with batched command dispatch.
    # While buffering is active, command() calls are accumulated locally.
    # On flush(), all accumulated commands are sent as a single IPC
    # round-trip, reducing latency during multi-step layout operations.

    def __init__(self) -> None:
        super().__init__()
        self._buffering = False
        self._buf: list[str] = list()

    def command(self, payload: str) -> list:  # type: ignore[override]
        if self._buffering:
            self._buf.append(payload)
            return []
        return super().command(payload)

    def start_buffering(self) -> None:
        # Begin accumulating subsequent command() calls.
        self._buffering = True

    def flush(self) -> list:
        # Send all buffered commands in one IPC call; disable buffering.
        self._buffering = False
        if not self._buf:
            return []
        payload = ';'.join(self._buf)
        self._buf.clear()
        return super().command(payload)

    def discard(self) -> None:
        # Discard buffered commands without sending; disable buffering.
        self._buffering = False
        self._buf.clear()

    def get_tree(self) -> Con:  # type: ignore[override]
        # Flush pending moves before reading tree to get a consistent view
        was_buffering = self._buffering
        self.flush()
        try:
            tree = super().get_tree()
        finally:
            if was_buffering:
                self.start_buffering()
        return tree  # type: ignore[return-value]

    def get_workspaces(self) -> list:  # type: ignore[override]
        was_buffering = self._buffering
        self.flush()
        try:
            workspaces = super().get_workspaces()
        finally:
            if was_buffering:
                self.start_buffering()
        return workspaces


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
    kind = _LAYOUT_BY_NAME.get(args[0])
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
_COMMANDS: dict[str, _CmdHandler] = {
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
# -------------------------------- Layout Engine ------------------------------
# -----------------------------------------------------------------------------
def _run_existing_layouts(i3: SwayConn) -> None:
    # Re-tile each currently existing workspace with managed state.
    # Snapshot focused workspace once to avoid cross-display focus theft
    focused_ws = _get_focused_workspace(i3)
    focused_ws_id: Optional[int] = focused_ws.id if focused_ws else None
    # Cache output rects once for portrait-detection on new workspaces
    outputs: dict[str, object] = {
        out.name: out   # type: ignore[attr-defined]
        for out in i3.get_outputs()
    }
    for reply in i3.get_workspaces():
        # Skip orphaned helper temp workspaces (e.g. __ws_B0, __swh_tmp_A1)
        # to prevent the layout engine from tiling transient/zombie workspaces.
        if any(reply.name.startswith(p) for p in _TEMP_WS_PREFIXES):
            continue
        ws_id = int(reply.ipc_data['id'])
        # On first encounter initialise with orientation-aware default.
        # Portrait outputs (height > width) default to 'stack' (1 column).
        if ws_id not in _ws_states:
            out = outputs.get(getattr(reply, 'output', ''))
            is_portrait = (out is not None
                           and out.rect.height   # type: ignore[union-attr]
                           > out.rect.width)     # type: ignore[union-attr]
            _get_ws_state(ws_id,
                          LayoutKind.STACK if is_portrait else None)
        try:
            _run_layout(i3, ws_id, focused_ws_id)
        except Exception:
            # One broken workspace must not abort layout for all others.
            # Discard stale commands and re-enable buffering for the next
            # workspace so it still benefits from command batching.
            i3.discard()
            i3.start_buffering()
            logging.exception('layout error on workspace %s', reply.name)


def _run_layout(i3: SwayConn, ws_id: int,
                focused_ws_id: Optional[int] = None) -> None:
    # Select and run the appropriate layout function for a workspace.
    state = _get_ws_state(ws_id)
    if state.kind == LayoutKind.NOP:
        _run_nop_layout(i3, state, focused_ws_id)
    else:
        _run_ncol_layout(i3, state, focused_ws_id)


def _run_nop_layout(i3: SwayConn, state: WorkspaceState,
                    focused_ws_id: Optional[int] = None) -> None:
    # Nop layout: preserve manual placement and current focus.
    ws = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return
    # Nothing to tile; preserve state so it survives until more windows open
    if len([leaf for leaf in ws.leaves() if not _is_floating(leaf)]) <= 1:
        return
    # Only restore focus for the focused workspace to avoid stealing focus
    # across displays during global reflow.
    if state.ws_id != focused_ws_id:
        return
    focused = ws.find_focused()
    if focused:
        focused.command('focus')


def _run_ncol_layout(i3: SwayConn, state: WorkspaceState,
                     focused_ws_id: Optional[int] = None) -> None:
    # NCol layout: deterministically rebalance the whole workspace tree.
    ws: Optional[Con] = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return
    # Nothing to tile; preserve state so it survives until more windows open
    n_tiling = len([leaf for leaf in ws.leaves() if not _is_floating(leaf)])
    if n_tiling <= 1:
        return

    is_focused = (state.ws_id == focused_ws_id)

    # Save focused window ID BEFORE reflow: _reflow_ncol uses move/swap
    # commands that can cause sway to silently shift focus to the master,
    # so ws.find_focused() called after the loop would return the wrong window.
    pre_focused = ws.find_focused()
    pre_focused_id: Optional[int] = (pre_focused.id
                                     if pre_focused is not None else None)

    # Scale the iteration cap with workspace size: one structural change per
    # pass means O(n_windows) passes in the worst case.
    max_iters = max(20, n_tiling * MAX_REFLOW_ITERS_PER_WIN)
    for _ in range(max_iters):
        ws = _refetch(i3, ws)
        if ws is None:
            return
        if not _reflow_ncol(i3, state, ws, is_focused):
            break
    else:
        logging.warning('reflow did not converge for ws %d (%d windows)',
                        state.ws_id, n_tiling)

    ws = _refetch(i3, ws)
    if ws and is_focused:
        # Restore focus to the window that was focused before the reflow
        focused = (i3.get_tree().find_by_id(pre_focused_id)
                   if pre_focused_id is not None else None)
        if focused is None:
            focused = ws.find_focused()
        if focused:
            _refocus_window(i3, focused)


def _reflow_ncol(i3: SwayConn, state: WorkspaceState,
                 ws_in: Con, is_focused: bool = False) -> bool:
    # Redistribute windows into n-column layout; return True if moved.
    # Columns are top-level vertical-split containers in the workspace.
    # The first column holds n_masters windows; each remaining column
    # holds an equal share of the slave windows.
    # For n_columns == 1 (stack layout) all windows stay in one column.
    ws: object = ws_in
    if len(ws.leaves()) <= 1:
        return False

    # Ensure all top-level containers use the correct split direction
    split_dir = _transform_cmd(state, 'splitv')
    for node in ws.nodes:
        if node.layout != split_dir:
            node.command(split_dir)
    ws = _refetch(i3, ws)
    if ws is None:
        return False

    # Stack layout: collapse extra columns into a single vertical column
    if state.n_columns <= 1:
        if len(ws.nodes) <= 1:
            return False
        node = ws.nodes[-1].nodes[0]
        _daemon_move_ids[node.id] = time.monotonic() + _MOVE_ID_TTL
        focused = ws.find_focused() if is_focused else None
        node.command('move left')
        if is_focused and focused:
            focused.command('focus')
        return True

    n_leaves = len(ws.leaves())
    n_slaves = max(0, n_leaves - state.n_masters)
    n_slave_cols = max(1, state.n_columns - 1)
    slaves_per_col = math.ceil(n_slaves / n_slave_cols) if n_slaves else 0

    nodes = _get_layout_nodes(state, ws)

    for i, col in enumerate(nodes):
        n = len(nodes)

        if i == 0:  # master pane
            if n == 1 and len(col.nodes) > state.n_masters:
                # Single column overflows: push one window right to new col
                _daemon_move_ids[col.nodes[-1].id] = (
                    time.monotonic() + _MOVE_ID_TTL)
                focused = ws.find_focused() if ws else None
                col.nodes[-1].command(_transform_cmd(state, 'move right'))
                if is_focused and focused:
                    focused.command('focus')
                return True
            if n > 1:
                if _balance_cols(i3, col, state.n_masters, nodes[1]):
                    return True

        elif i == n - 1 and i > 0:  # last slave pane
            if i > 1:
                if _balance_cols(i3, nodes[i - 1], slaves_per_col, col):
                    return True
            if len(col.nodes) > 1:
                if n < state.n_columns:
                    # Too few columns: expand rightward
                    _daemon_move_ids[col.nodes[-1].id] = (
                        time.monotonic() + _MOVE_ID_TTL)
                    focused = ws.find_focused() if ws else None
                    col.nodes[-1].command(
                        _transform_cmd(state, 'move right'))
                    if is_focused and focused:
                        focused.command('focus')
                    return True
                elif n > state.n_columns:
                    # Too many columns: collapse leftward
                    _daemon_move_ids[col.nodes[0].id] = (
                        time.monotonic() + _MOVE_ID_TTL)
                    focused = ws.find_focused() if ws else None
                    col.nodes[0].command(
                        _transform_cmd(state, 'move left'))
                    if is_focused and focused:
                        focused.command('focus')
                    return True

        else:  # middle slave pane
            if i + 1 < n:
                if _balance_cols(i3, col, slaves_per_col, nodes[i + 1]):
                    return True

    return False


# -----------------------------------------------------------------------------
# ----------------------------- Transformations -------------------------------
# -----------------------------------------------------------------------------
def _apply_transpose(i3: SwayConn, ws: Con) -> None:
    # Physically transpose the workspace container tree.
    focused = _get_focused_window(i3)
    _transpose_container(i3, ws)
    if focused:
        focused.command('focus')


def _apply_reflectx(i3: SwayConn, ws: Con) -> None:
    # Flip the workspace horizontally (mirror left/right).
    _reflect_container(i3, ws, {'splith'})


def _apply_reflecty(i3: SwayConn, ws: Con) -> None:
    # Flip the workspace vertically (mirror top/bottom).
    _reflect_container(i3, ws, {'splitv'})


def _transpose_container(i3: SwayConn, con: Con) -> None:
    # Recursively toggle split direction and rotate the container tree.
    if con.type == 'workspace' and con.nodes:
        con.nodes[0].command('layout toggle split')
        # These moves rotate the top-level split without moving window content
        if con.layout == 'splith':
            con.nodes[0].command('move up')
        elif con.layout == 'splitv':
            con.nodes[0].command('move left')
        _reverse_nodes(i3, con, start=1)
    elif con.nodes:
        con.nodes[0].command('layout toggle split')
    for child in con.nodes:
        _transpose_container(i3, child)


def _reflect_container(i3: SwayConn, con: Con,
                       split_filter: set[str]) -> None:
    # Recursively reverse child order in containers matching split_filter.
    if con.layout in split_filter:
        _reverse_nodes(i3, con)
    for child in con.nodes:
        _reflect_container(i3, child, split_filter)


def _transform_cmd(state: WorkspaceState, cmd: str) -> str:
    # Map a sway direction/split command through active transformations.
    if Transform.TRANSPOSE in state.transforms:
        cmd = _transpose_map_cmd(cmd)
    if Transform.REFLECTX in state.transforms:
        cmd = _reflectx_map_cmd(cmd)
    if Transform.REFLECTY in state.transforms:
        cmd = _reflecty_map_cmd(cmd)
    return cmd


def _transpose_map_cmd(cmd: str) -> str:
    # Rotate direction/split commands 90 degrees clockwise.
    parts = cmd.split()
    if parts[0] == 'move':
        dirs = {'right': 'down', 'down': 'left',
                'left': 'up', 'up': 'right'}
        return f'move {dirs.get(parts[1], parts[1])}'
    splits = {
        'splitv': 'splith', 'splith': 'splitv',
        'split v': 'split h', 'split h': 'split v',
        'split vertical': 'split horizontal',
        'split horizontal': 'split vertical',
    }
    return splits.get(cmd, cmd)


def _reflectx_map_cmd(cmd: str) -> str:
    # Mirror left/right direction commands.
    parts = cmd.split()
    if parts[0] == 'move':
        dirs = {'right': 'left', 'left': 'right'}
        return f'move {dirs.get(parts[1], parts[1])}'
    return cmd


def _reflecty_map_cmd(cmd: str) -> str:
    # Mirror up/down direction commands.
    parts = cmd.split()
    if parts[0] == 'move':
        dirs = {'up': 'down', 'down': 'up'}
        return f'move {dirs.get(parts[1], parts[1])}'
    return cmd


# -----------------------------------------------------------------------------
# ------------------------------- Window Cycling ------------------------------
# -----------------------------------------------------------------------------
def _find_offset_window(win: Optional[Con],
                        offset: int) -> Optional[Con]:
    # Return the leaf at cyclic offset from win within its workspace.
    if win is None:
        return None
    ws = win.workspace()
    if ws is None:
        return None
    leaves = ws.leaves()
    if not leaves:
        return None
    ids = [leaf.id for leaf in leaves]
    try:
        idx = ids.index(win.id)
    except ValueError:
        return None  # win is a floating container
    return leaves[(idx + offset) % len(leaves)]


def _refocus_window(i3: SwayConn, win: Con) -> None:
    # Re-focus win, moving the mouse cursor to its center.
    # Temporarily focusing a neighbor and returning to win causes sway
    # to warp the cursor to the window's center rather than leaving it
    # at a window border.
    _focus_window(i3, offset=+1, win=win)
    win.command('focus')
    if win.fullscreen_mode == 1:
        win.command('fullscreen')


def _focus_window(i3: SwayConn, offset: int,
                  win: Optional[Con] = None) -> None:
    # Focus the leaf at cyclic offset from win (or the focused window).
    src = win or _get_focused_window(i3)
    if src is None:
        return
    target = _find_offset_window(src, offset)
    if target is None:
        return
    target.command('focus')
    if src.fullscreen_mode == 1:
        target.command('fullscreen')


def _swap_window(i3: SwayConn, offset: int,
                 win: Optional[Con] = None,
                 focus_after: bool = True) -> None:
    # Swap win (or focused) with the leaf at cyclic offset.
    src = win or _get_focused_window(i3)
    if src is None:
        return
    target = _find_offset_window(src, offset)
    if target is None:
        return
    src.command(f'swap container with con_id {target.id}')
    if focus_after:
        src.command('focus')
        if src.fullscreen_mode == 1:
            target.command('fullscreen')


def _swap_new_window(i3: SwayConn, new_win_id: int) -> None:
    # Swap the newly opened window before its sway-assigned predecessor.
    # Sway places new windows immediately after the focused window, so
    # leaves()[idx-1] is the previously focused window.  Swapping puts
    # the new window in the focused window's slot (before it).
    # Calling i3.get_tree() flushes any pending buffered commands first,
    # giving a consistent pre-reflow view of the tree.
    tree = i3.get_tree()
    new_win = tree.find_by_id(new_win_id)
    if new_win is None or _is_floating(new_win):
        return
    ws = new_win.workspace()
    if ws is None:
        return
    leaves = [leaf for leaf in ws.leaves() if not _is_floating(leaf)]
    if len(leaves) < 2:
        return
    ids = [leaf.id for leaf in leaves]
    try:
        idx = ids.index(new_win_id)
    except ValueError:
        return
    if idx == 0:
        return  # already before all other windows; nothing to do
    if idx == len(leaves) - 1:
        return  # predecessor was the last window; keep new window at end
    prev_win = leaves[idx - 1]
    new_win.command(f'swap container with con_id {prev_win.id}')
    new_win.command('focus')


def _swap_moved_window(i3: SwayConn, moved_win_id: int) -> None:
    # Swap the moved window before the active window on the destination ws.
    # Sway places moved containers immediately after the focused window, so
    # leaves()[idx-1] is the previously focused window.  Swapping puts the
    # moved window in the focused window's slot (before it).
    # Unlike _swap_new_window, this always swaps even when the moved
    # window lands at the last position, preserving the intended stack order.
    tree = i3.get_tree()
    moved_win = tree.find_by_id(moved_win_id)
    if moved_win is None or _is_floating(moved_win):
        return
    ws = moved_win.workspace()
    if ws is None:
        return
    # Skip helper-initiated evacuation moves to temp workspaces.
    # Moving to __swh_tmp_* is done by fix_workspace_order in the helper;
    # focusing those windows would steal focus from the (possibly empty)
    # new_ws, triggering sway's auto-deletion of new_ws.
    if any(ws.name.startswith(p) for p in _TEMP_WS_PREFIXES):
        return
    leaves = [leaf for leaf in ws.leaves() if not _is_floating(leaf)]
    if len(leaves) < 2:
        return
    ids = [leaf.id for leaf in leaves]
    try:
        idx = ids.index(moved_win_id)
    except ValueError:
        return
    if idx == 0:
        return  # already before all other windows; nothing to do
    prev_win = leaves[idx - 1]
    moved_win.command(f'swap container with con_id {prev_win.id}')
    moved_win.command('focus')


# -----------------------------------------------------------------------------
# ----------------------------- Master Operations -----------------------------
# -----------------------------------------------------------------------------
def _get_layout_nodes(state: WorkspaceState, ws: Con) -> list[Con]:
    # Return top-level layout nodes in the logical master-to-slave order.
    flip = ((Transform.REFLECTX in state.transforms
             and ws.layout == 'splith')
            or (Transform.REFLECTY in state.transforms
                and ws.layout == 'splitv'))
    return list(ws.nodes[::-1] if flip else ws.nodes)


def _get_master_windows(ws: Con, state: WorkspaceState) -> list[Con]:
    # Return windows that belong to the logical master pane.
    leaves = [node for node in ws.leaves() if not _is_floating(node)]
    if not leaves:
        return []
    if state.kind == LayoutKind.NOP:
        return [leaves[0]]
    nodes = _get_layout_nodes(state, ws)
    if not nodes:
        return [leaves[0]]
    master_col = nodes[0]
    master_leaves = [node for node in master_col.leaves()
                     if not _is_floating(node)]
    if not master_leaves:
        return [leaves[0]]
    return master_leaves


def _get_master_window(ws: Con, state: WorkspaceState) -> Optional[Con]:
    # Return the primary master window for commands like focus/promote.
    master_windows = _get_master_windows(ws, state)
    if not master_windows:
        return None
    return master_windows[0]


def _get_resize_target(ws: Con, state: WorkspaceState) -> Optional[Con]:
    # Return a stable resize target within the master pane.
    master_windows = _get_master_windows(ws, state)
    if not master_windows:
        return None
    focused = ws.find_focused()
    if focused is not None:
        for node in master_windows:
            if node.id == focused.id:
                return node
    return master_windows[0]


# -----------------------------------------------------------------------------
# --------------------------- Container Move Helpers --------------------------
# -----------------------------------------------------------------------------
def _move_container(src: Con, dst: Con) -> None:
    # Teleport src to be placed adjacent to dst using a temporary mark.
    _daemon_move_ids[src.id] = time.monotonic() + _MOVE_ID_TTL
    dst.command(f'mark {MOVE_MARK}')
    src.command(f'move window to mark {MOVE_MARK}')
    dst.command(f'unmark {MOVE_MARK}')


def _prepend_child(i3: SwayConn, container: Con, node: Con) -> None:
    # Prepend node to the front of container's children via pairwise swaps.
    _move_container(node, container)
    # node landed at the end; bubble it to position 0
    for old_node in container.nodes[::-1]:
        node.command(f'swap container with con_id {old_node.id}')


def _reverse_nodes(i3: SwayConn, con: Con, start: int = 0) -> None:
    # Reverse child order of con from index start onward, using swaps.
    nodes = con.nodes
    half = math.ceil((len(nodes) - start) / 2)
    for i, node in enumerate(nodes[start:start + half]):
        mirror = nodes[-(i + 1)]
        if node.id != mirror.id:
            node.command(f'swap container with con_id {mirror.id}')


def _balance_cols(i3: SwayConn, col1: Con, expected: int,
                  col2: Con) -> bool:
    # Balance windows between adjacent columns; return True if moved.
    # Transfers one window from col2 -> col1 when col1 is short, or
    # col1 -> front-of-col2 when col1 exceeds the expected count.
    if len(col1.nodes) < expected and col2.nodes:
        _move_container(col2.nodes[0], col1)
        return True
    if len(col1.nodes) > expected and len(col1.nodes) > 1:
        _prepend_child(i3, col2, col1.nodes[-1])
        return True
    return False


# -----------------------------------------------------------------------------
# ----------------------- Sway Tree Utilities / State -------------------------
# -----------------------------------------------------------------------------
def _get_focused_workspace(i3: SwayConn) -> Optional[Con]:
    # Return the currently focused workspace container.
    for reply in i3.get_workspaces():
        if reply.focused:
            node = i3.get_tree().find_by_id(reply.ipc_data['id'])
            return node.workspace() if node else None
    return None


def _get_focused_window(i3: SwayConn) -> Optional[Con]:
    # Return the currently focused leaf window.
    ws = _get_focused_workspace(i3)
    return ws.find_focused() if ws else None


def _refetch(i3: SwayConn,
             con: Optional[Con]) -> Optional[Con]:
    # Re-fetch a container from the live sway tree by its id.
    if con is None:
        return None
    return i3.get_tree().find_by_id(con.id)


def _is_floating(con: Con) -> bool:
    # Return True if the container is floating.
    return (con.floating in ('user_on', 'auto_on')
            or con.type == 'floating_con')


def _get_ws_state(ws_id: int,
                  default_kind: Optional[LayoutKind] = None
                  ) -> WorkspaceState:
    # Return (creating if absent) the layout state for a workspace.
    # ``default_kind`` sets the initial layout kind when creating the state for
    # the first time.  When None the global DEFAULT_LAYOUT name is used.
    if ws_id not in _ws_states:
        kind = (default_kind
                if default_kind is not None
                else _LAYOUT_BY_NAME.get(DEFAULT_LAYOUT, LayoutKind.TALL))
        _ws_states[ws_id] = WorkspaceState(ws_id=ws_id, kind=kind)
    return _ws_states[ws_id]


def _parse_nop_commands(event: BindingEvent) -> list[list[str]]:
    # Extract nop command arg lists from a sway binding event.
    # The binding command may chain multiple subcommands with ';' or ','.
    # Only subcommands starting with 'nop' are extracted; others are ignored.
    tokens = shlex.split(event.binding.command)
    result: list[list[str]] = list()
    i = 0
    while i < len(tokens):
        if tokens[i] in (';', ','):
            i += 1
            continue
        if tokens[i] == 'nop':
            j = i + 1
            while j < len(tokens) and tokens[j] not in (';', ','):
                j += 1
            if j > i + 1:
                result.append(tokens[i + 1:j])
            i = j
        else:
            # Skip non-nop subcommands
            while i < len(tokens) and tokens[i] not in (';', ','):
                i += 1
    return result


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
def main() -> None:
    global DEFAULT_LAYOUT
    parser = argparse.ArgumentParser(
        description='Sway IPC tiling daemon (swaymonad replacement).')
    parser.add_argument(
        '--default-layout', default=DEFAULT_LAYOUT,
        choices=list(_LAYOUT_BY_NAME),
        help='Layout for workspaces with no explicit setting.')
    parser.add_argument(
        '--verbose', '-v', action='count',
        help='Enable debug logging (repeat for more verbose output).')
    parser.add_argument(
        '--log-file',
        help='Write log to file instead of stderr.')
    args = parser.parse_args()
    DEFAULT_LAYOUT = args.default_layout

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.WARNING,
        filename=args.log_file,
        format='%(asctime)s %(levelname)-8s [%(filename)s:%(lineno)d]'
               ' %(message)s')

    i3 = SwayConn()
    i3.on(i3ipc.Event.BINDING, cast(_IpcHandler, on_binding))
    i3.on(i3ipc.Event.WINDOW_NEW, cast(_IpcHandler, on_window))
    i3.on(i3ipc.Event.WINDOW_CLOSE, cast(_IpcHandler, on_window))
    i3.on(i3ipc.Event.WINDOW_MOVE, cast(_IpcHandler, on_window))
    i3.main()


if __name__ == '__main__':
    main()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
