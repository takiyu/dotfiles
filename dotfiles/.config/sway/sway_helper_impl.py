#!/usr/bin/env python3
'''Sway IPC tiling daemon.

Self-contained replacement for the swaymonad dependency.
Listens for sway 'nop' binding events and manages auto-tiling layouts.
Invoke once at startup:  exec_always ~/.config/sway/sway_helper_impl.py
'''
import argparse
import enum
import logging
import math
import shlex
import traceback
from typing import Optional

import i3ipc


# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
_MOVE_MARK = '__tiler_mark'   # Temporary sway mark for container relocation
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
        # Snapshot of the workspace tree captured before each window event
        self.prev_ws: Optional[i3ipc.Con] = None

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

    def __init__(self, *args, **kwargs) -> None:
        super().__init__(*args, **kwargs)
        self._buffering = False
        self._buf: list[str] = []

    def command(self, payload: str) -> list:
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

    def get_tree(self) -> i3ipc.Con:
        # Flush pending moves before reading tree to get a consistent view
        was_buffering = self._buffering
        self.flush()
        tree = super().get_tree()
        if was_buffering:
            self.start_buffering()
        return tree

    def get_workspaces(self) -> list:
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
def on_binding(i3: SwayConn, event: i3ipc.Event) -> None:
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


def on_window(i3: SwayConn, event: i3ipc.Event) -> None:
    '''Apply the workspace layout after any window new/close/move event.'''
    try:
        ws = (_get_workspace_of_event(i3, event)
              or _get_focused_workspace(i3))
        if ws is None:
            return
        i3.start_buffering()
        _run_layout(i3, ws.id, event)
        i3.flush()
    except Exception:
        traceback.print_exc()


# -----------------------------------------------------------------------------
# ----------------------------- Command Handlers ------------------------------
# -----------------------------------------------------------------------------
def _cmd_promote_window(i3: SwayConn,
                        event: i3ipc.Event, *args: str) -> None:
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
                      event: i3ipc.Event, *args: str) -> None:
    '''Move focus to the master (largest) window.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    master = _find_biggest_window(ws)
    if master:
        master.command('focus')


def _cmd_resize_master(i3: SwayConn,
                       event: i3ipc.Event, *args: str) -> None:
    '''Resize the master window. Extra args forwarded to sway resize.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    master = _find_biggest_window(ws)
    if master:
        master.command('resize ' + ' '.join(args))


def _cmd_focus_next_window(i3: SwayConn,
                           event: i3ipc.Event, *args: str) -> None:
    _focus_window(i3, offset=+1)


def _cmd_focus_prev_window(i3: SwayConn,
                           event: i3ipc.Event, *args: str) -> None:
    _focus_window(i3, offset=-1)


def _cmd_swap_next_window(i3: SwayConn,
                          event: i3ipc.Event, *args: str) -> None:
    _swap_with_window(i3, offset=+1)


def _cmd_swap_prev_window(i3: SwayConn,
                          event: i3ipc.Event, *args: str) -> None:
    _swap_with_window(i3, offset=-1)


def _cmd_set_layout(i3: SwayConn,
                    event: i3ipc.Event, *args: str) -> None:
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
    _run_layout(i3, ws.id, None)


def _cmd_increment_masters(i3: SwayConn,
                           event: i3ipc.Event, *args: str) -> None:
    '''Add one master slot to the current workspace and re-tile.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters += 1
    _run_layout(i3, ws.id, None)


def _cmd_decrement_masters(i3: SwayConn,
                           event: i3ipc.Event, *args: str) -> None:
    '''Remove one master slot (minimum 1) and re-tile.'''
    ws = _get_focused_workspace(i3)
    if ws is None:
        return
    state = _get_ws_state(ws.id)
    state.n_masters = max(1, state.n_masters - 1)
    _run_layout(i3, ws.id, None)


def _cmd_toggle_transform(i3: SwayConn, event: i3ipc.Event,
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
    _run_layout(i3, ws.id, None)


def _cmd_transpose(i3: SwayConn,
                   event: i3ipc.Event, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.TRANSPOSE)


def _cmd_reflectx(i3: SwayConn,
                  event: i3ipc.Event, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.REFLECTX)


def _cmd_reflecty(i3: SwayConn,
                  event: i3ipc.Event, *args: str) -> None:
    _cmd_toggle_transform(i3, event, Transform.REFLECTY)


def _cmd_move(i3: SwayConn,
              event: i3ipc.Event, *args: str) -> None:
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
                    event: i3ipc.Event, *args: str) -> None:
    '''Toggle fullscreen and update the workspace layout snapshot.'''
    ws = _get_focused_workspace(i3)
    i3.command('fullscreen')
    if ws:
        ws_fresh = _refetch(i3, ws)
        if ws_fresh:
            _get_ws_state(ws.id).prev_ws = ws_fresh


# Map nop command names to their handler functions
_COMMANDS: dict[str, object] = {
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
def _run_layout(i3: SwayConn, ws_id: int,
                event: Optional[i3ipc.Event]) -> None:
    '''Select and run the appropriate layout function for a workspace.'''
    state = _get_ws_state(ws_id)
    if state.kind == LayoutKind.NOP:
        _run_nop_layout(i3, state, event)
    else:
        _run_ncol_layout(i3, state, event)


def _run_nop_layout(i3: SwayConn, state: WorkspaceState,
                    event: Optional[i3ipc.Event]) -> None:
    '''Nop layout: no auto-tiling; re-layout source workspace on moves.'''
    ws = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return
    if event and event.change == 'move':
        _relayout_old_workspace(i3, ws)
    focused = ws.find_focused()
    if focused:
        focused.command('focus')


def _run_ncol_layout(i3: SwayConn, state: WorkspaceState,
                     event: Optional[i3ipc.Event]) -> None:
    '''NCol layout: maintain n-column tiling on every window change.'''
    ws = i3.get_tree().find_by_id(state.ws_id)
    if ws is None:
        return
    if state.prev_ws is None:
        state.prev_ws = ws

    should_reflow = (event is None)
    focused_ws_name: Optional[str] = None

    if event and event.change == 'new':
        ws = _refetch(i3, ws)
        old_ids = {n.id for n in state.prev_ws.leaves()}
        new_ids = {n.id for n in ws.leaves()}
        if old_ids != new_ids:
            # Place new window before the currently focused one
            new_con = ws.find_by_id(event.container.id)
            _swap_with_window(i3, offset=-1, win=new_con)
            should_reflow = True

    elif event and event.change == 'close':
        old_ids = {n.id for n in state.prev_ws.leaves()}
        new_ids = {n.id for n in ws.leaves()}
        focused_ws = _get_focused_workspace(i3)
        closed = state.prev_ws.find_by_id(event.container.id)
        if (old_ids != new_ids
                and focused_ws is not None
                and focused_ws.id == state.ws_id
                and closed is not None
                and not _is_floating(closed)):
            should_reflow = True
            # Focus the window that followed the closed one in cycle order
            next_win = closed
            for _ in range(len(old_ids)):
                next_win = _find_offset_window(next_win, +1)
                if next_win and next_win.id in new_ids:
                    next_win.command('focus')
                    break

    elif event and event.change == 'move':
        if _move_count[0] > 0:
            # This move was triggered by the layout engine; skip reflow
            _move_count[0] -= 1
            return
        should_reflow = True
        focused_ws = _get_focused_workspace(i3)
        if focused_ws:
            focused_ws_name = focused_ws.name
        win = ws.find_by_id(event.container.id)
        _swap_with_window(i3, offset=-1, win=win, focus_after=False)
        _relayout_old_workspace(i3, ws)

    # Iteratively reflow until the layout is stable
    while should_reflow:
        ws = _refetch(i3, ws)
        if ws is None:
            break
        should_reflow = _reflow_ncol(i3, state, ws)

    # Restore focus to the workspace active before the reflow
    if focused_ws_name:
        i3.command(f'workspace {focused_ws_name}')

    # Move the mouse cursor to the center of the focused window
    ws = _refetch(i3, ws)
    focused_ws = _get_focused_workspace(i3)
    if ws and focused_ws and ws.id == focused_ws.id:
        focused = ws.find_focused()
        if focused:
            _refocus_window(i3, focused)

    ws = _refetch(i3, ws)
    if ws:
        state.prev_ws = ws


def _reflow_ncol(i3: SwayConn, state: WorkspaceState,
                 ws: i3ipc.Con) -> bool:
    '''Redistribute windows into n-column layout; return True if moved.

    Columns are top-level vertical-split containers in the workspace.
    The first column holds n_masters windows; each remaining column
    holds an equal share of the slave windows.
    '''
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
                focused = ws.find_focused()
                col.nodes[0].command(_transform_cmd(state, 'move left'))
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
                    focused = ws.find_focused()
                    col.nodes[-1].command(
                        _transform_cmd(state, 'move right'))
                    if focused:
                        focused.command('focus')
                    caused = True
                    ws = _refetch(i3, ws)
                elif n > state.n_columns:
                    # Too many columns: collapse leftward
                    _move_count[0] += 1
                    focused = ws.find_focused()
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
def _apply_transpose(i3: SwayConn, ws: i3ipc.Con) -> None:
    '''Physically transpose the workspace container tree.'''
    focused = _get_focused_window(i3)
    _transpose_container(i3, ws)
    if focused:
        focused.command('focus')


def _apply_reflectx(i3: SwayConn, ws: i3ipc.Con) -> None:
    '''Flip the workspace horizontally (mirror left/right).'''
    _reflect_container(i3, ws, {'splith'})


def _apply_reflecty(i3: SwayConn, ws: i3ipc.Con) -> None:
    '''Flip the workspace vertically (mirror top/bottom).'''
    _reflect_container(i3, ws, {'splitv'})


def _transpose_container(i3: SwayConn, con: i3ipc.Con) -> None:
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


def _reflect_container(i3: SwayConn, con: i3ipc.Con,
                       split_filter: set) -> None:
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
def _find_offset_window(win: i3ipc.Con,
                        offset: int) -> Optional[i3ipc.Con]:
    '''Return the leaf at cyclic offset from win within its workspace.'''
    leaves = win.workspace().leaves()
    if not leaves:
        return None
    ids = [leaf.id for leaf in leaves]
    try:
        idx = ids.index(win.id)
    except ValueError:
        return None  # win is a floating container
    return leaves[(idx + offset) % len(leaves)]


def _refocus_window(i3: SwayConn, win: i3ipc.Con) -> None:
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
                  win: Optional[i3ipc.Con] = None) -> None:
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
                      win: Optional[i3ipc.Con] = None,
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
def _find_biggest_window(ws: i3ipc.Con) -> Optional[i3ipc.Con]:
    '''Return the non-floating leaf with the largest screen area.'''
    leaves = [n for n in ws.leaves() if not _is_floating(n)]
    if not leaves:
        return None
    return max(leaves, key=lambda n: n.rect.width * n.rect.height)


# -----------------------------------------------------------------------------
# --------------------------- Container Move Helpers --------------------------
# -----------------------------------------------------------------------------
def _move_container(src: i3ipc.Con, dst: i3ipc.Con) -> None:
    '''Teleport src to be placed adjacent to dst using a temporary mark.'''
    _move_count[0] += 1
    dst.command(f'mark {_MOVE_MARK}')
    src.command(f'move window to mark {_MOVE_MARK}')
    dst.command(f'unmark {_MOVE_MARK}')


def _add_to_front(i3: SwayConn, container: i3ipc.Con,
                  node: i3ipc.Con) -> None:
    '''Prepend node to the front of container's children via pairwise swaps.'''
    _move_container(node, container)
    # node landed at the end; bubble it to position 0
    for old_node in container.nodes[::-1]:
        node.command(f'swap container with con_id {old_node.id}')


def _reverse_nodes(i3: SwayConn, con: i3ipc.Con, start: int = 0) -> None:
    '''Reverse child order of con from index start onward, using swaps.'''
    nodes = con.nodes
    half = math.ceil((len(nodes) - start) / 2)
    for i, node in enumerate(nodes[start:start + half]):
        mirror = nodes[-(i + 1)]
        if node.id != mirror.id:
            node.command(f'swap container with con_id {mirror.id}')


def _balance_cols(i3: SwayConn, col1: i3ipc.Con, expected: int,
                  col2: i3ipc.Con) -> bool:
    '''Balance windows between adjacent columns; return True if moved.

    Transfers one window from col2 → col1 when col1 is short, or
    col1 → front-of-col2 when col1 exceeds the expected count.
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
def _get_focused_workspace(i3: SwayConn) -> Optional[i3ipc.Con]:
    '''Return the currently focused workspace container.'''
    for reply in i3.get_workspaces():
        if reply.focused:
            node = i3.get_tree().find_by_id(reply.ipc_data['id'])
            return node.workspace() if node else None
    return None


def _get_focused_window(i3: SwayConn) -> Optional[i3ipc.Con]:
    '''Return the currently focused leaf window.'''
    ws = _get_focused_workspace(i3)
    return ws.find_focused() if ws else None


def _get_workspace_of_event(i3: SwayConn,
                             event: i3ipc.Event) -> Optional[i3ipc.Con]:
    '''Return the workspace that contains the event's container.'''
    node = i3.get_tree().find_by_id(event.container.id)
    return node.workspace() if node else None


def _refetch(i3: SwayConn,
             con: Optional[i3ipc.Con]) -> Optional[i3ipc.Con]:
    '''Re-fetch a container from the live sway tree by its id.'''
    if con is None:
        return None
    return i3.get_tree().find_by_id(con.id)


def _is_floating(con: i3ipc.Con) -> bool:
    '''Return True if the container is floating.'''
    return (con.floating in ('user_on', 'auto_on')
            or con.type == 'floating_con')


def _relayout_old_workspace(i3: SwayConn, new_ws: i3ipc.Con) -> None:
    '''Re-tile the workspace that a container moved FROM.'''
    old_ws = _get_focused_workspace(i3)
    if old_ws is None:
        return
    if old_ws.id == new_ws.id:
        # Cross-output move: navigate back temporarily to find the source ws
        i3.command('workspace back_and_forth')
        old_ws = _get_focused_workspace(i3)
        i3.command('workspace back_and_forth')
    if old_ws:
        _run_layout(i3, old_ws.id, None)


def _get_ws_state(ws_id: int) -> WorkspaceState:
    '''Return (creating if absent) the layout state for a workspace.'''
    if ws_id not in _ws_states:
        kind = _LAYOUT_BY_NAME.get(DEFAULT_LAYOUT, LayoutKind.TALL)
        _ws_states[ws_id] = WorkspaceState(ws_id=ws_id, kind=kind)
    return _ws_states[ws_id]


def _parse_nop_commands(event: i3ipc.Event) -> list[list[str]]:
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
    i3.on(i3ipc.Event.BINDING, on_binding)
    i3.on(i3ipc.Event.WINDOW_NEW, on_window)
    i3.on(i3ipc.Event.WINDOW_CLOSE, on_window)
    i3.on(i3ipc.Event.WINDOW_MOVE, on_window)
    i3.main()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    main()
