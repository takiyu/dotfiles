import math
import time
from typing import Optional

from swayhelper.constants import (MOVE_ID_TTL, MOVE_MARK, TEMP_WS_PREFIXES,
                                  Con, LayoutKind, Transform)
from swayhelper.ipc import SwayConn
from swayhelper.state import WorkspaceState, daemon_move_ids
from swayhelper.tree_utils import _get_focused_window, _is_floating


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
    if any(ws.name.startswith(p) for p in TEMP_WS_PREFIXES):
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
    daemon_move_ids[src.id] = time.monotonic() + MOVE_ID_TTL
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
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
