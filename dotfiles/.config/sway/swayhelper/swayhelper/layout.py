import logging
import math
import time
from typing import Optional

from swayhelper.constants import (MAX_REFLOW_ITERS_PER_WIN, MOVE_ID_TTL,
                                  TEMP_WS_PREFIXES, Con, LayoutKind, Transform)
from swayhelper.ipc import SwayConn
from swayhelper.state import WorkspaceState, daemon_move_ids, ws_states
from swayhelper.tree_utils import (_get_focused_window, _get_focused_workspace,
                                   _get_ws_state, _is_floating, _refetch)
from swayhelper.window_ops import (_balance_cols, _get_layout_nodes,
                                   _refocus_window, _reverse_nodes)


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
        if any(reply.name.startswith(p) for p in TEMP_WS_PREFIXES):
            continue
        ws_id = int(reply.ipc_data['id'])
        # On first encounter initialise with orientation-aware default.
        # Portrait outputs (height > width) default to 'stack' (1 column).
        if ws_id not in ws_states:
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
    pre_focused_id: Optional[int]
    if pre_focused is not None:
        pre_focused_id = pre_focused.id
    else:
        pre_focused_id = None

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
        daemon_move_ids[node.id] = time.monotonic() + MOVE_ID_TTL
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
                expiry = time.monotonic() + MOVE_ID_TTL
                daemon_move_ids[col.nodes[-1].id] = expiry
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
                    expiry = time.monotonic() + MOVE_ID_TTL
                    daemon_move_ids[col.nodes[-1].id] = expiry
                    focused = ws.find_focused() if ws else None
                    cmd = _transform_cmd(state, 'move right')
                    col.nodes[-1].command(cmd)
                    if is_focused and focused:
                        focused.command('focus')
                    return True
                elif n > state.n_columns:
                    # Too many columns: collapse leftward
                    expiry = time.monotonic() + MOVE_ID_TTL
                    daemon_move_ids[col.nodes[0].id] = expiry
                    focused = ws.find_focused() if ws else None
                    cmd = _transform_cmd(state, 'move left')
                    col.nodes[0].command(cmd)
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
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
