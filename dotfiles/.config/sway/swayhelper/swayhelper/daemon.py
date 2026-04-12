'''Sway IPC tiling daemon.

Self-contained replacement for the swaymonad dependency.
Listens for sway 'nop' binding events and manages auto-tiling layouts.
Invoke once at startup:  exec_always ~/.config/sway/sway_helper_daemon.py
'''
import argparse
import enum
import logging
import math
import shlex
import traceback
from typing import Any, Callable, Optional, cast

import i3ipc
import i3ipc.events

from swayhelper.constants import MOVE_MARK

# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
DEFAULT_LAYOUT = 'tall'       # Layout for workspaces with no explicit setting


# -----------------------------------------------------------------------------
# ----------------------------------- Types -----------------------------------
# -----------------------------------------------------------------------------
class LayoutKind(enum.Enum):
    TALL = 'tall'
    THREE_COL = '3_col'
    NOP = 'nop'


class Transform(enum.Enum):
    TRANSPOSE = 'TRANSPOSE'
    REFLECTX = 'REFLECTX'
    REFLECTY = 'REFLECTY'


# Number of tiling columns per layout kind (NOP has no column-based tiling)
_LAYOUT_COLS: dict[LayoutKind, int] = {
    LayoutKind.TALL: 2,
    LayoutKind.THREE_COL: 3,
}

# String-to-enum map for use with the 'set_layout' command
_LAYOUT_BY_NAME: dict[str, LayoutKind] = {k.value: k for k in LayoutKind}

# i3ipc type aliases; Con attrs are set dynamically via setattr, so alias to
# Any to avoid false positives from pyright when stubs are not present.
Con = Any
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
    '''Tiling layout state for a single sway workspace.'''

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
        '''Number of tiling columns for this layout kind.'''
        return _LAYOUT_COLS.get(self.kind, 1)


# Per-workspace layout states, keyed by workspace con_id
_ws_states: dict[int, WorkspaceState] = {}

# Tracks moves queued by the layout engine to suppress resulting WINDOW_MOVE
# events that would otherwise trigger infinite reflow cycles.
_move_count: list[int] = [0]


# -----------------------------------------------------------------------------
# ------------------------------ IPC Connection -------------------------------
# -----------------------------------------------------------------------------
class SwayConn(i3ipc.Connection):
    '''Sway IPC connection with batched command dispatch.

    While buffering is active, command() calls are accumulated locally.
    On flush(), all accumulated commands are sent as a single IPC
    round-trip, reducing latency during multi-step layout operations.
    '''

    def __init__(self) -> None:
        super().__init__()
        self._buffering = False
        self._buf: list[str] = []

    def command(self, payload: str) -> list:  # type: ignore[override]
        if self._buffering:
            self._buf.append(payload)
            return []
        return super().command(payload)

    def start_buffering(self) -> None:
        '''Begin accumulating subsequent command() calls.'''
        self._buffering = True

    def flush(self) -> list:
        '''Send all buffered commands in one IPC call; disable buffering.'''
        self._buffering = False
        if not self._buf:
            return []
        payload = ';'.join(self._buf)
        self._buf.clear()
        return super().command(payload)

    def get_tree(self) -> Con:  # type: ignore[override]
        # Flush pending moves before reading tree to get a consistent view
        was_buffering = self._buffering
        self.flush()
        tree = super().get_tree()
        if was_buffering:
            self.start_buffering()
        return tree  # type: ignore[return-value]

    def get_workspaces(self) -> list:  # type: ignore[override]
        was_buffering = self._buffering
        self.flush()
        workspaces = super().get_workspaces()
        if was_buffering:
            self.start_buffering()
        return workspaces


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ------------------------------ Implementation -------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# ----------------------------- Event Dispatchers -----------------------------
# -----------------------------------------------------------------------------
def on_binding(i3: SwayConn, event: BindingEvent) -> None:
    '''Dispatch 'nop' binding events to registered command handlers.'''
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
        traceback.print_exc()


def on_window(i3: SwayConn, event: WindowEvent) -> None:
    '''Re-tile existing workspaces after any relevant window event.'''
    try:
        if event.change == 'move' and _move_count[0] > 0:
            _move_count[0] -= 1
            return
        i3.start_buffering()
        _run_existing_layouts(i3)
        i3.flush()
    except Exception:
        traceback.print_exc()


# -----------------------------------------------------------------------------
# ----------------------------- Command Handlers ------------------------------
# -----------------------------------------------------------------------------
def _cmd_promote_window(i3: SwayConn,
                        event: BindingEvent, *args: str) -> None:
    '''Swap the focused window with the master (largest) window.'''
    ws = _get_focused_workspace(i3)
    focused = _get_focused_window(i3)
    if ws is None or focused is None:
        return
    master = _find_biggest_window(ws)
    if master is None or focused.id == master.id:
        return
    focused.command(f'swap container with con_id {master.id}')
    focused.command('focus')


def _cmd_focus_master(i3: SwayConn,
                      event: BindingEvent, *args: str) -> None:
    '''Move focus to the master (largest) window.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    master = _find_biggest_window(ws)
    if master:
        master.command('focus')


def _cmd_resize_master(i3: SwayConn,
                       event: BindingEvent, *args: str) -> None:
    '''Resize the master window. Extra args forwarded to sway resize.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    master = _find_biggest_window(ws)
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
    _swap_with_window(i3, offset=+1)


def _cmd_swap_prev_window(i3: SwayConn,
                          event: BindingEvent, *args: str) -> None:
    _swap_with_window(i3, offset=-1)


def _cmd_set_layout(i3: SwayConn,
                    event: BindingEvent, *args: str) -> None:
    '''Change the current workspace to a named layout and re-tile.'''
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
    '''Add one master slot to the current workspace and re-tile.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters += 1
    _run_layout(i3, ws.id)


def _cmd_decrement_masters(i3: SwayConn,
                           event: BindingEvent, *args: str) -> None:
    '''Remove one master slot (minimum 1) and re-tile.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters = max(1, state.n_masters - 1)
    _run_layout(i3, ws.id)


def _cmd_toggle_transform(i3: SwayConn, event: BindingEvent,
                          transform: Transform) -> None:
    '''Toggle a geometric transformation and re-tile the workspace.'''
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
    '''Move the focused window in a direction with layout awareness.'''
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
    '''Toggle fullscreen on the focused window.'''
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
    '''Re-tile each currently existing workspace with managed state.'''
    for reply in i3.get_workspaces():
        _run_layout(i3, int(reply.ipc_data['id']))


def _run_layout(i3: SwayConn, ws_id: int) -> None:
    '''Select and run the appropriate layout function for a workspace.'''
    state = _get_ws_state(ws_id)
    if state.kind == LayoutKind.NOP:
        _run_nop_layout(i3, state)
    else:
        _run_ncol_layout(i3, state)


def _run_nop_layout(i3: SwayConn, state: WorkspaceState) -> None:
    '''Nop layout: preserve manual placement and current focus.'''
    ws = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return
    focused = ws.find_focused()
    if focused:
        focused.command('focus')


def _run_ncol_layout(i3: SwayConn, state: WorkspaceState) -> None:
    '''NCol layout: deterministically rebalance the whole workspace tree.'''
    ws: Optional[Con] = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return

    while True:
        ws = _refetch(i3, ws)
        if ws is None:
            return
        if not _reflow_ncol(i3, state, ws):
            break

    ws = _refetch(i3, ws)
    focused_ws = _get_focused_workspace(i3)
    if ws and focused_ws and ws.id == focused_ws.id:
        focused = ws.find_focused()
        if focused:
            _refocus_window(i3, focused)


def _reflow_ncol(i3: SwayConn, state: WorkspaceState,
                 ws_in: Con) -> bool:
    '''Redistribute windows into n-column layout; return True if moved.

    Columns are top-level vertical-split containers in the workspace.
    The first column holds n_masters windows; each remaining column
    holds an equal share of the slave windows.
    '''
    # Declare Optional to allow reassignment via _refetch within this function
    ws: Any = ws_in
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

    n_leaves = len(ws.leaves())
    n_slaves = max(0, n_leaves - state.n_masters)
    n_slave_cols = max(1, state.n_columns - 1)
    slaves_per_col = math.ceil(n_slaves / n_slave_cols) if n_slaves else 0

    # Reverse column traversal order when a horizontal/vertical reflect is on
    flip = ((Transform.REFLECTX in state.transforms
             and ws.layout == 'splith')
            or (Transform.REFLECTY in state.transforms
                and ws.layout == 'splitv'))
    nodes = ws.nodes[::-1] if flip else list(ws.nodes)

    caused = False
    for i, col in enumerate(nodes):
        n = len(nodes)

        if i == 0:  # master pane
            if n == 1 and len(col.nodes) > state.n_masters:
                # Single column overflows: push one window right to new col
                _move_count[0] += 1
                focused = ws.find_focused() if ws else None
                if focused:
                    focused.command('focus')
                caused = True
                ws = _refetch(i3, ws)
            if n > 1:
                caused |= _balance_cols(i3, col, state.n_masters, nodes[1])

        elif i == n - 1 and i > 0:  # last slave pane
            if i > 1:
                caused |= _balance_cols(
                    i3, nodes[i - 1], slaves_per_col, col)
            if len(col.nodes) > 1:
                if n < state.n_columns:
                    # Too few columns: expand rightward
                    _move_count[0] += 1
                    focused = ws.find_focused() if ws else None
                    col.nodes[-1].command(
                        _transform_cmd(state, 'move right'))
                    if focused:
                        focused.command('focus')
                    caused = True
                    ws = _refetch(i3, ws)
                elif n > state.n_columns:
                    # Too many columns: collapse leftward
                    _move_count[0] += 1
                    focused = ws.find_focused() if ws else None
                    col.nodes[0].command(
                        _transform_cmd(state, 'move left'))
                    if focused:
                        focused.command('focus')
                    caused = True
                    ws = _refetch(i3, ws)

        else:  # middle slave pane
            if i + 1 < n:
                caused |= _balance_cols(
                    i3, col, slaves_per_col, nodes[i + 1])

    return caused


# -----------------------------------------------------------------------------
# ----------------------------- Transformations -------------------------------
# -----------------------------------------------------------------------------
def _apply_transpose(i3: SwayConn, ws: Con) -> None:
    '''Physically transpose the workspace container tree.'''
    focused = _get_focused_window(i3)
    _transpose_container(i3, ws)
    if focused:
        focused.command('focus')


def _apply_reflectx(i3: SwayConn, ws: Con) -> None:
    '''Flip the workspace horizontally (mirror left/right).'''
    _reflect_container(i3, ws, {'splith'})


def _apply_reflecty(i3: SwayConn, ws: Con) -> None:
    '''Flip the workspace vertically (mirror top/bottom).'''
    _reflect_container(i3, ws, {'splitv'})


def _transpose_container(i3: SwayConn, con: Con) -> None:
    '''Recursively toggle split direction and rotate the container tree.'''
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
    '''Recursively reverse child order in containers matching split_filter.'''
    if con.layout in split_filter:
        _reverse_nodes(i3, con)
    for child in con.nodes:
        _reflect_container(i3, child, split_filter)


def _transform_cmd(state: WorkspaceState, cmd: str) -> str:
    '''Map a sway direction/split command through active transformations.'''
    if Transform.TRANSPOSE in state.transforms:
        cmd = _transpose_map_cmd(cmd)
    if Transform.REFLECTX in state.transforms:
        cmd = _reflectx_map_cmd(cmd)
    if Transform.REFLECTY in state.transforms:
        cmd = _reflecty_map_cmd(cmd)
    return cmd


def _transpose_map_cmd(cmd: str) -> str:
    '''Rotate direction/split commands 90 degrees clockwise.'''
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
    '''Mirror left/right direction commands.'''
    parts = cmd.split()
    if parts[0] == 'move':
        dirs = {'right': 'left', 'left': 'right'}
        return f'move {dirs.get(parts[1], parts[1])}'
    return cmd


def _reflecty_map_cmd(cmd: str) -> str:
    '''Mirror up/down direction commands.'''
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
    '''Return the leaf at cyclic offset from win within its workspace.'''
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
    '''Re-focus win, moving the mouse cursor to its center.

    Temporarily focusing a neighbor and returning to win causes sway
    to warp the cursor to the window's center rather than leaving it
    at a window border.
    '''
    _focus_window(i3, offset=+1, win=win)
    win.command('focus')
    if win.fullscreen_mode == 1:
        win.command('fullscreen')


def _focus_window(i3: SwayConn, offset: int,
                  win: Optional[Con] = None) -> None:
    '''Focus the leaf at cyclic offset from win (or the focused window).'''
    src = win or _get_focused_window(i3)
    if src is None:
        return
    target = _find_offset_window(src, offset)
    if target is None:
        return
    target.command('focus')
    if src.fullscreen_mode == 1:
        target.command('fullscreen')


def _swap_with_window(i3: SwayConn, offset: int,
                      win: Optional[Con] = None,
                      focus_after: bool = True) -> None:
    '''Swap win (or focused) with the leaf at cyclic offset.'''
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


# -----------------------------------------------------------------------------
# ----------------------------- Master Operations -----------------------------
# -----------------------------------------------------------------------------
def _find_biggest_window(ws: Con) -> Optional[Con]:
    '''Return the non-floating leaf with the largest screen area.'''
    leaves = [n for n in ws.leaves() if not _is_floating(n)]
    if not leaves:
        return None
    return max(leaves, key=lambda n: n.rect.width * n.rect.height)


# -----------------------------------------------------------------------------
# --------------------------- Container Move Helpers --------------------------
# -----------------------------------------------------------------------------
def _move_container(src: Con, dst: Con) -> None:
    '''Teleport src to be placed adjacent to dst using a temporary mark.'''
    _move_count[0] += 1
    dst.command(f'mark {MOVE_MARK}')
    src.command(f'move window to mark {MOVE_MARK}')
    dst.command(f'unmark {MOVE_MARK}')


def _add_to_front(i3: SwayConn, container: Con, node: Con) -> None:
    '''Prepend node to the front of container's children via pairwise swaps.'''
    _move_container(node, container)
    # node landed at the end; bubble it to position 0
    for old_node in container.nodes[::-1]:
        node.command(f'swap container with con_id {old_node.id}')


def _reverse_nodes(i3: SwayConn, con: Con, start: int = 0) -> None:
    '''Reverse child order of con from index start onward, using swaps.'''
    nodes = con.nodes
    half = math.ceil((len(nodes) - start) / 2)
    for i, node in enumerate(nodes[start:start + half]):
        mirror = nodes[-(i + 1)]
        if node.id != mirror.id:
            node.command(f'swap container with con_id {mirror.id}')


def _balance_cols(i3: SwayConn, col1: Con, expected: int,
                  col2: Con) -> bool:
    '''Balance windows between adjacent columns; return True if moved.

    Transfers one window from col2 -> col1 when col1 is short, or
    col1 -> front-of-col2 when col1 exceeds the expected count.
    '''
    if len(col1.nodes) < expected and col2.nodes:
        _move_container(col2.nodes[0], col1)
        col1.nodes.append(col2.nodes.pop(0))
        return True
    if len(col1.nodes) > expected and len(col1.nodes) > 1:
        _add_to_front(i3, col2, col1.nodes[-1])
        col2.nodes.insert(0, col1.nodes.pop(-1))
        return True
    return False


# -----------------------------------------------------------------------------
# ----------------------- Sway Tree Utilities / State -------------------------
# -----------------------------------------------------------------------------
def _get_focused_workspace(i3: SwayConn) -> Optional[Con]:
    '''Return the currently focused workspace container.'''
    for reply in i3.get_workspaces():
        if reply.focused:
            node = i3.get_tree().find_by_id(reply.ipc_data['id'])
            return node.workspace() if node else None
    return None


def _get_focused_window(i3: SwayConn) -> Optional[Con]:
    '''Return the currently focused leaf window.'''
    ws = _get_focused_workspace(i3)
    return ws.find_focused() if ws else None


def _refetch(i3: SwayConn,
             con: Optional[Con]) -> Optional[Con]:
    '''Re-fetch a container from the live sway tree by its id.'''
    if con is None:
        return None
    return i3.get_tree().find_by_id(con.id)


def _is_floating(con: Con) -> bool:
    '''Return True if the container is floating.'''
    return (con.floating in ('user_on', 'auto_on')
            or con.type == 'floating_con')


def _get_ws_state(ws_id: int) -> WorkspaceState:
    '''Return (creating if absent) the layout state for a workspace.'''
    if ws_id not in _ws_states:
        kind = _LAYOUT_BY_NAME.get(DEFAULT_LAYOUT, LayoutKind.TALL)
        _ws_states[ws_id] = WorkspaceState(ws_id=ws_id, kind=kind)
    return _ws_states[ws_id]


def _parse_nop_commands(event: BindingEvent) -> list[list[str]]:
    '''Extract nop command arg lists from a sway binding event.

    The binding command may chain multiple subcommands with ';' or ','.
    Only subcommands starting with 'nop' are extracted; others are ignored.
    '''
    tokens = shlex.split(event.binding.command)
    result: list[list[str]] = []
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


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    main()
