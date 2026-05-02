# Sway workspace and display navigation helper.
# Provides CLI commands for workspace/display focus and move operations.
# Invoke as: python3 -m swayhelper.helper <action> [--opt <value>]
import argparse
import contextlib
import fcntl
import json
import os
import os.path as osp
import re
import subprocess
import time
from typing import Optional

# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
# Persistent file (per-user, survives sway reload but cleared on reboot)
WS_DISP_MAP_FILE = f'/tmp/sway_wsdisp_map_{os.getuid()}.json'
# Lock file to serialise concurrent fix_workspace_order invocations
_REORDER_LOCK_FILE = f'/tmp/sway_wsreorder_{os.getuid()}.lock'
# Lock file to serialise concurrent move_nei_workspace invocations
_MOVE_LOCK_FILE = f'/tmp/sway_wsmove_{os.getuid()}.lock'
# Temporary workspace prefix used during workspace order correction
_WS_REORDER_TMP = '__swh_tmp_'
# Temporary workspace prefix used during setup_workspace_names pass 1
_WS_SETUP_TMP = '__ws_'


# -----------------------------------------------------------------------------
# ---------------------------- Action Entry Point -----------------------------
# -----------------------------------------------------------------------------
def run_action(action: str, opt: str = '') -> None:
    if action == 'setup':
        setup_workspace_names()
    elif action == 'focus_next_workspace':
        focus_nei_workspace(+1)
    elif action == 'focus_prev_workspace':
        focus_nei_workspace(-1)
    elif action == 'move_next_workspace':
        move_nei_workspace(+1)
    elif action == 'move_prev_workspace':
        move_nei_workspace(-1)
    elif action == 'focus_next_valid_workspace':
        focus_valid_nei_workspace(+1)
    elif action == 'focus_prev_valid_workspace':
        focus_valid_nei_workspace(-1)
    elif action == 'move_next_valid_workspace':
        move_valid_nei_workspace(+1)
    elif action == 'move_prev_valid_workspace':
        move_valid_nei_workspace(-1)
    elif action == 'insert_workspace_before_current':
        insert_workspace_before_current()
    elif action == 'delete_current_workspace_if_empty':
        delete_current_workspace_if_empty()
    elif action == 'compact_workspaces':
        compact_workspaces()
    elif action == 'focus_next_display':
        focus_nei_display(+1)
    elif action == 'focus_prev_display':
        focus_nei_display(-1)
    elif action == 'move_next_display':
        move_nei_display(+1)
    elif action == 'move_prev_display':
        move_nei_display(-1)
    elif action == 'focus_display':
        focus_display(int(opt))
    elif action == 'move_display':
        move_display(int(opt))
    else:
        raise ValueError(f'Invalid action: {action}')


# -----------------------------------------------------------------------------
# --------------------- High-level Actions Implementation ---------------------
# -----------------------------------------------------------------------------
def setup_workspace_names() -> None:
    # Rename workspaces to A0, A1, B0, B1, etc. based on display map.
    all_disps = get_displays()
    # Ensure persistent display-letter map is up to date
    disp_to_letter = _update_display_map(all_disps)
    # Recover windows from any orphaned temp workspaces before restoration
    _cleanup_temp_workspaces(all_disps)
    # Move any misplaced workspaces back to their home display
    _restore_workspace_assignments(all_disps, disp_to_letter)
    # Pass 1: rename to tmp names, preserving original workspace numbers
    for disp in all_disps:
        letter = disp_to_letter[disp]
        native_ws = [w for w in get_workspaces(disp)
                     if _is_native_ws(w, letter)]
        # Collect numbers already in use by letter-prefixed names
        used_nums = set()
        for w in native_ws:
            m = re.match(rf'^{letter}(\d+)$', w)
            if m:
                used_nums.add(int(m.group(1)))
        next_free = 0
        for old_name in native_ws:
            m = re.match(rf'^{letter}(\d+)$', old_name)
            if m:
                num = int(m.group(1))  # Preserve original number
            else:
                # Default sway numeric ws: assign next free slot
                while next_free in used_nums:
                    next_free += 1
                num = next_free
                used_nums.add(num)
                next_free += 1
            rename_workspace(old_name, f'{_WS_SETUP_TMP}{letter}{num}')
    # Pass 2: strip the tmp prefix to restore final names
    for disp in all_disps:
        letter = disp_to_letter[disp]
        prefix = f'{_WS_SETUP_TMP}{letter}'
        for ws_name in get_workspaces_raw(disp):
            if ws_name.startswith(prefix):
                rename_workspace(ws_name, ws_name[len(_WS_SETUP_TMP):])


def _restore_workspace_assignments(all_disps: list[str],
                                   disp_to_letter: dict[str, str]) -> None:
    # Move workspaces with known letter prefixes to their home display.
    active_outputs = set(all_disps)
    # Build reverse map: letter -> output (only for currently active displays)
    letter_to_disp = {v: k for k, v in disp_to_letter.items()
                      if k in active_outputs}
    ws_info = _get_all_workspaces_with_output()
    orig_focused = get_cur_workspace()
    moved_any = False
    for ws_name, current_output in ws_info:
        m = re.match(r'^([A-Z])(\d+)$', ws_name)
        if not m:
            continue
        letter = m.group(1)
        if letter not in letter_to_disp:
            continue  # Home display not currently active
        target_output = letter_to_disp[letter]
        if current_output == target_output:
            continue  # Already on correct display
        focus_workspace(ws_name)
        run_cmd(f"swaymsg 'move workspace to output {target_output}'")
        moved_any = True
    if moved_any:
        focus_workspace(orig_focused)


def _cleanup_temp_workspaces(all_disps: list[str]) -> None:
    # Move windows from orphaned temp workspaces to their real targets.
    # Handles both the current _WS_REORDER_TMP prefix and the legacy _t
    # prefix left by older versions, enabling recovery on restart.
    for disp in all_disps:
        for ws_name in list(get_workspaces_raw(disp)):
            real_name = _strip_reorder_tmp_prefix(ws_name)
            if real_name == ws_name or not _is_managed_workspace(real_name):
                continue
            try:
                for win_id in get_windows_on_workspace(ws_name):
                    move_window_to_workspace(win_id, real_name)
            except Exception:
                pass  # Best-effort; leave windows in place on IPC error


def focus_nei_workspace(offset: int = 1) -> None:
    # Focus to neighbor workspace on current display.
    cur_disp = get_cur_display()
    cur_ws = get_cur_workspace(cur_disp)
    nxt_ws = _shift_workspace_name(cur_ws, offset)
    if nxt_ws == cur_ws:
        if offset >= 0:
            return
        # Wrap: moving left from ws 0 jumps to the last existing workspace
        workspaces = get_workspaces(cur_disp)
        if not workspaces or workspaces[-1] == cur_ws:
            return
        nxt_ws = workspaces[-1]
    # Record existing workspaces before creation
    ws_before = set(get_workspaces_raw(cur_disp))
    focus_workspace(nxt_ws)
    # Fix sway's internal order if a new workspace was created
    if nxt_ws not in ws_before:
        fix_workspace_order(cur_disp, nxt_ws)


def move_nei_workspace(offset: int = 1) -> None:
    # Move to neighbor workspace on current display.
    # Serialised by _move_lock so rapid keypresses don't race: the second
    # invocation waits until the first (including fix_workspace_order's focus
    # shuffling) finishes, then re-reads the correct post-move state.
    with _move_lock():
        cur_disp = get_cur_display()
        cur_ws = get_cur_workspace(cur_disp)
        nxt_ws = _shift_workspace_name(cur_ws, offset)
        if nxt_ws == cur_ws:
            if offset >= 0:
                return
            # Wrap: moving left from ws 0 jumps to the last existing workspace
            workspaces = get_workspaces(cur_disp)
            if not workspaces or workspaces[-1] == cur_ws:
                return
            nxt_ws = workspaces[-1]
        # Record existing workspaces before creation
        ws_before = set(get_workspaces_raw(cur_disp))
        win_id = move_workspace(nxt_ws)
        # Fix sway's internal order if a new workspace was created
        if nxt_ws not in ws_before:
            fix_workspace_order(cur_disp, nxt_ws)
            # fix_workspace_order ends with focus_workspace(new_ws);
            # focus the moved window explicitly to ensure correct focus.
            if win_id is not None:
                focus_window(win_id)


def fix_workspace_order(display: str, new_ws: str) -> None:
    # Reorder sway's workspace list after a new one is created mid-sequence.
    # When B4 is created after B0,B3,B5, sway appends it: [B0,B3,B5,B4].
    # We evacuate out-of-order workspaces (B5) to temp names in sorted order
    # so each temp workspace is appended right after new_ws.  Then we rename
    # them back in place — no focus change required, so new_ws can never be
    # auto-deleted (sway only deletes empty workspaces on focus-away).
    # Concurrent calls are serialised by a file lock; state is re-read under
    # the lock so each invocation operates on a consistent snapshot.
    with _reorder_lock():
        ws_actual = get_workspaces_raw(display)
        if new_ws not in ws_actual:
            return
        new_ws_pos = ws_actual.index(new_ws)

        # Only consider managed workspaces to prevent _t*-name cascade.
        # Read windows now to filter out empty workspaces — empty ones have
        # no temp created for them and would cause poll timeouts.
        candidates = [ws for ws in ws_actual[:new_ws_pos]
                      if ws_sort_key(ws) > ws_sort_key(new_ws)
                      and _is_managed_workspace(ws)]
        if not candidates:
            return
        saved_windows = {ws: get_windows_on_workspace(ws) for ws in candidates}
        ws_to_reorder = [ws for ws in candidates if saved_windows[ws]]
        if not ws_to_reorder:
            return

        # Pre-clean any stale temp workspaces from prior crashes.
        # Stale temps sit at an unknown position (not end of list), so reusing
        # them would break the sorted-append invariant of the rename approach.
        stale_tmps = {f'{_WS_REORDER_TMP}{ws}' for ws in ws_to_reorder}
        if set(get_workspaces_raw(display)) & stale_tmps:
            for ws in sorted(ws_to_reorder, key=ws_sort_key):
                tmp = f'{_WS_REORDER_TMP}{ws}'
                for win_id in get_windows_on_workspace(tmp):
                    move_window_to_workspace(win_id, ws)
            for _ in range(10):
                if not (set(get_workspaces_raw(display)) & stale_tmps):
                    break
                time.sleep(0.02)
            else:
                return  # Stale temps still present; abort to avoid mis-order

        # Evacuate in sorted order: sway appends each new temp workspace at
        # the end, so sorted evacuation places temps in correct positions.
        for ws in sorted(ws_to_reorder, key=ws_sort_key):
            for win_id in saved_windows[ws]:
                move_window_to_workspace(win_id, f'{_WS_REORDER_TMP}{ws}')

        # Poll until out-of-order workspaces are auto-deleted (usually instant)
        evacuated = False
        for _ in range(15):
            if not (set(get_workspaces_raw(display)) & set(ws_to_reorder)):
                evacuated = True
                break
            time.sleep(0.02)

        if not evacuated:
            # Timed out: restore windows to original workspaces and abort
            for ws in ws_to_reorder:
                for win_id in saved_windows[ws]:
                    move_window_to_workspace(win_id, ws)
            return

        # Rename temps back — preserves positions, no focus change needed.
        for ws in sorted(ws_to_reorder, key=ws_sort_key):
            rename_workspace(f'{_WS_REORDER_TMP}{ws}', ws)
        # Re-focus new_ws only if it still exists (guard: don't recreate it)
        if new_ws in get_workspaces_raw(display):
            focus_workspace(new_ws)


def focus_valid_nei_workspace(offset: int = 1) -> None:
    # Focus to valid neighbor workspace on current display.
    cur_disp = get_cur_display()
    ws, cur_ws = get_workspace_cycle(cur_disp)
    if not ws or cur_ws not in ws:
        return
    nxt_ws = ws[(ws.index(cur_ws) + offset) % len(ws)]
    focus_workspace(nxt_ws)


def move_valid_nei_workspace(offset: int = 1) -> None:
    # Move focused window to valid neighbor workspace on current display.
    # Only targets workspaces that already exist; no new workspace is created.
    # Serialised by _move_lock to avoid focus-state races on rapid keypresses.
    with _move_lock():
        cur_disp = get_cur_display()
        ws, cur_ws = get_workspace_cycle(cur_disp)
        if not ws or cur_ws not in ws:
            return
        nxt_ws = ws[(ws.index(cur_ws) + offset) % len(ws)]
        if nxt_ws == cur_ws:
            return
        move_workspace(nxt_ws)


def insert_workspace_before_current() -> None:
    # Insert a new workspace immediately before the current workspace.
    cur_disp = get_cur_display()
    ws_data = _get_workspaces_data()
    cur_ws = _get_visible_workspace_name(ws_data, cur_disp)
    ws_parts = _split_workspace_name(cur_ws)
    if ws_parts is None:
        return
    prefix, cur_index = ws_parts
    workspaces = _get_managed_workspace_names(ws_data, cur_disp)
    affected = [ws_name for ws_name in workspaces
                if _should_shift_workspace(ws_name, prefix, cur_index)]
    for ws_name in sorted(affected, key=ws_sort_key, reverse=True):
        rename_workspace(ws_name, _shift_workspace_name(ws_name, +1))
    inserted_ws = f'{prefix}{cur_index}'
    focus_workspace(inserted_ws)
    fix_workspace_order(cur_disp, inserted_ws)


def delete_current_workspace_if_empty() -> None:
    # Delete an empty workspace by shifting later workspaces left.
    cur_disp = get_cur_display()
    ws_data = _get_workspaces_data()
    cur_ws = _get_visible_workspace_name(ws_data, cur_disp)
    workspaces = _get_managed_workspace_names(ws_data, cur_disp)
    if cur_ws not in workspaces or len(workspaces) <= 1:
        return
    if 0 < len(get_windows_on_workspace(cur_ws)):
        return
    ws_parts = _split_workspace_name(cur_ws)
    if ws_parts is None:
        return
    prefix, cur_index = ws_parts
    later = [ws_name for ws_name in workspaces
             if _should_shift_workspace(ws_name, prefix, cur_index + 1)]
    if later:
        focus_workspace(later[0])
        for ws_name in later:
            rename_workspace(ws_name, _shift_workspace_name(ws_name, -1))
        return
    prev_ws = workspaces[(workspaces.index(cur_ws) - 1) % len(workspaces)]
    if prev_ws != cur_ws:
        focus_workspace(prev_ws)


def compact_workspaces() -> None:
    # Delete all empty workspaces on the current display and renumber
    # survivors to consecutive indices per prefix group.
    with _reorder_lock():
        cur_disp = get_cur_display()
        ws_data = _get_workspaces_data()
        workspaces = _get_managed_workspace_names(ws_data, cur_disp)
        if not workspaces:
            return

        # Partition into empty and non-empty workspaces
        empty_set = {ws for ws in workspaces
                     if not get_windows_on_workspace(ws)}
        survivors = [ws for ws in workspaces if ws not in empty_set]

        if not survivors:
            return  # refuse to delete every workspace

        cur_ws = _get_visible_workspace_name(ws_data, cur_disp)

        # If the current workspace is empty, switch to first survivor so sway
        # auto-deletes cur_ws when focus leaves it.
        if cur_ws in empty_set:
            anchor = survivors[0]
            focus_workspace(anchor)
        else:
            anchor = cur_ws

        # Visit each remaining empty workspace then return to anchor;
        # sway auto-deletes empty workspaces on focus loss.
        for empty in sorted(empty_set - {cur_ws}, key=ws_sort_key):
            focus_workspace(empty)
            focus_workspace(anchor)

        # Re-read post-deletion state; also covers the no-empty case
        # (actual_survivors == survivors when nothing was deleted).
        ws_data2 = _get_workspaces_data()
        current_names = set(_get_managed_workspace_names(ws_data2, cur_disp))
        actual_survivors = [ws for ws in survivors if ws in current_names]

        # Rename survivors to consecutive indices per prefix group.
        # Iterate lowest source index first; rename target is always free.
        prefix_groups: dict[str, list[str]] = dict()
        for ws in actual_survivors:
            parts = _split_workspace_name(ws)
            if parts is None:
                continue
            prefix, _ = parts
            prefix_groups.setdefault(prefix, []).append(ws)

        for prefix, group in prefix_groups.items():
            for new_idx, ws_name in enumerate(
                    sorted(group, key=ws_sort_key)):
                new_name = f'{prefix}{new_idx}'
                if ws_name != new_name:
                    rename_workspace(ws_name, new_name)


def focus_nei_display(offset: int = 1) -> None:
    # Focus to neighbor display.
    all_disps = get_displays()
    cur_disp = get_cur_display()
    nxt_disp = all_disps[(all_disps.index(cur_disp) + offset) % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    focus_workspace(nxt_ws)


def move_nei_display(offset: int = 1) -> None:
    # Move to neighbor display.
    all_disps = get_displays()
    cur_disp = get_cur_display()
    nxt_disp = all_disps[(all_disps.index(cur_disp) + offset) % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    move_workspace(nxt_ws)


def focus_display(index: int) -> None:
    # Focus to display by index.
    all_disps = get_displays()
    nxt_disp = all_disps[index % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    focus_workspace(nxt_ws)


def move_display(index: int) -> None:
    # Move to display by index.
    all_disps = get_displays()
    nxt_disp = all_disps[index % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    move_workspace(nxt_ws)


# -----------------------------------------------------------------------------
# ---------------------------- Sway Getter Wrappers ---------------------------
# -----------------------------------------------------------------------------
def get_displays() -> list[str]:
    # Return active display names sorted by x then y position.
    outputs = json.loads(run_cmd("swaymsg -t get_outputs"))
    display_positions = [(output['name'],
                          output['rect']['x'],
                          output['rect']['y'])
                         for output in outputs if output['active']]
    display_positions.sort(key=lambda x: (x[1], x[2]))
    return [name for name, _, _ in display_positions]


def get_cur_display() -> str:
    return run_cmd("swaymsg -t get_outputs | jq -r '.[] | " +
                   "select(.focused) | .name'").strip()


def get_workspaces(display: Optional[str] = None) -> list[str]:
    # Get workspaces sorted by name.
    return sorted(get_workspaces_raw(display), key=ws_sort_key)


def get_workspaces_raw(display: Optional[str] = None) -> list[str]:
    # Get workspaces in sway's natural (creation) order.
    workspaces = _get_workspaces_data()
    names: list[str] = list()
    for ws in workspaces:
        if display is not None and ws.get('output') != display:
            continue
        names.append(str(ws['name']))
    return names


def _get_all_workspaces_with_output() -> list[tuple[str, str]]:
    # Return (workspace_name, output_name) pairs for all workspaces.
    workspaces = _get_workspaces_data()
    return [(ws['name'], ws['output']) for ws in workspaces]


def get_windows_on_workspace(ws_name: str) -> list:
    # Get all window container IDs on a workspace (recursive).
    tree = json.loads(run_cmd('swaymsg -t get_tree'))
    ws_node = _find_node(tree,
                         lambda n: n.get('type') == 'workspace'
                         and n.get('name') == ws_name)
    if ws_node is None:
        return []
    return _collect_windows(ws_node)


def _find_node(node: dict, predicate) -> Optional[dict]:
    # Find first node matching predicate in sway tree (DFS).
    if predicate(node):
        return node
    children = node.get('nodes', []) + node.get('floating_nodes', [])
    for child in children:
        result = _find_node(child, predicate)
        if result is not None:
            return result
    return None


def _collect_windows(node: dict) -> list:
    # Collect all leaf window con_ids under a node.
    children = node.get('nodes', []) + node.get('floating_nodes', [])
    if not children:
        return [node['id']] if node.get('type') == 'con' else []
    result = list()
    for child in children:
        result.extend(_collect_windows(child))
    return result


def _collect_tiling_windows(node: dict) -> list[int]:
    # Collect tiling-only leaf window con_ids under a node (DFS).
    children = node.get('nodes', [])
    if not children:
        return [node['id']] if node.get('type') == 'con' else []
    result: list[int] = list()
    for child in children:
        result.extend(_collect_tiling_windows(child))
    return result


def _get_next_window_on_cur_ws(win_id: int) -> Optional[int]:
    # Return the next tiling window after win_id on the current workspace.
    # Returns the previous window if win_id is the last one, or None if it
    # is the only tiling window on the workspace.
    tree = json.loads(run_cmd('swaymsg -t get_tree'))
    ws_node = _find_node(tree,
                         lambda n: n.get('type') == 'workspace'
                         and win_id in _collect_tiling_windows(n))
    if ws_node is None:
        return None
    ids = _collect_tiling_windows(ws_node)
    try:
        idx = ids.index(win_id)
    except ValueError:
        return None
    if len(ids) < 2:
        return None
    # Return next; fall back to previous when win_id is last
    return ids[idx + 1] if idx + 1 < len(ids) else ids[idx - 1]


def get_cur_workspace(display: Optional[str] = None) -> str:
    workspaces = _get_workspaces_data()
    if display is None:
        return _get_focused_workspace_name(workspaces)
    return _get_visible_workspace_name(workspaces, display)


def get_focused_window_id() -> Optional[int]:
    # Return con_id of the currently focused window, or None.
    tree = json.loads(run_cmd('swaymsg -t get_tree'))
    node = _find_node(tree, lambda n: n.get(
        'focused') and n.get('type') == 'con')
    return node['id'] if node is not None else None


# -----------------------------------------------------------------------------
# ---------------------------- Sway Action Wrappers ---------------------------
# -----------------------------------------------------------------------------
def rename_workspace(old_name: str, new_name: str) -> None:
    run_cmd(f"swaymsg 'rename workspace {old_name} to {new_name}'")


def focus_workspace(ws: str) -> None:
    run_cmd(f"swaymsg 'workspace {ws}'")


def move_workspace(ws: str) -> Optional[int]:
    # Combine move + workspace-switch + focus into one atomic swaymsg call.
    # Sway is single-threaded for IPC, so the daemon's layout re-tile
    # (triggered by WINDOW_MOVE) can only query the tree AFTER all three
    # sub-commands have been applied, guaranteeing it sees our window focused
    # instead of the master on the destination workspace.
    win_id = get_focused_window_id()
    if win_id is None:
        run_cmd(f"swaymsg 'move container to workspace {ws}'")
        focus_workspace(ws)
        return None
    next_win_id = _get_next_window_on_cur_ws(win_id)
    if next_win_id is not None:
        # After the move we are still on the source workspace; focusing
        # next_win_id here sets the source-ws focus without switching.
        run_cmd(f"swaymsg 'move container to workspace {ws}; "
                f"[con_id={next_win_id}] focus; "
                f"workspace {ws}; [con_id={win_id}] focus'")
    else:
        run_cmd(f"swaymsg 'move container to workspace {ws}; "
                f"workspace {ws}; [con_id={win_id}] focus'")
    return win_id


def move_window_to_workspace(win_id: int, ws_name: str) -> None:
    # Move a specific window by con_id to a target workspace.
    run_cmd(f"swaymsg '[con_id={win_id}] move to workspace {ws_name}'")


def focus_window(win_id: int) -> None:
    # Focus a specific window by con_id.
    run_cmd(f"swaymsg '[con_id={win_id}] focus'")


# -----------------------------------------------------------------------------
# ---------------------------------- Utility ----------------------------------
# -----------------------------------------------------------------------------
def ws_sort_key(name: str) -> tuple:
    # Sort key for workspace names like A0, B1, C2.
    match = re.match(r'([A-Z]*)(\d+)', name)
    if match:
        return (match.group(1), int(match.group(2)))
    return (name, 0)


def _split_workspace_name(ws_name: str) -> Optional[tuple[str, int]]:
    # Split workspace name into prefix and numeric suffix.
    match = re.match(r'^([A-Z]*)(\d+)$', ws_name)
    if match is None:
        return None
    return match.group(1), int(match.group(2))


def _shift_workspace_name(ws_name: str, offset: int) -> str:
    # Shift a workspace suffix without wrapping to a fixed maximum.
    ws_parts = _split_workspace_name(ws_name)
    if ws_parts is None:
        return ws_name
    prefix, index = ws_parts
    next_index = max(0, index + offset)
    return f'{prefix}{next_index}'


def _should_shift_workspace(ws_name: str, prefix: str,
                            min_index: int) -> bool:
    # Return True when a workspace must move right for insertion.
    ws_parts = _split_workspace_name(ws_name)
    if ws_parts is None:
        return False
    ws_prefix, ws_index = ws_parts
    return ws_prefix == prefix and min_index <= ws_index


def get_workspace_cycle(display: str) -> tuple[list[str], str]:
    # Return managed workspaces and the visible workspace for a display.
    # Sway can briefly expose a stale current workspace right after a close or
    # workspace deletion. Retry once so workspace-cycle bindings remain
    # single-shot.
    workspaces: list[str] = list()
    current = ''
    for _ in range(2):
        ws_data = _get_workspaces_data()
        workspaces = _get_managed_workspace_names(ws_data, display)
        current = _get_visible_workspace_name(ws_data, display)
        if current in workspaces:
            return workspaces, current
        time.sleep(0.05)
    return workspaces, current


def _get_managed_workspace_names(workspaces: list[dict[str, object]],
                                 display: Optional[str] = None) -> list[str]:
    # Return managed workspace names from one consistent snapshot.
    names: list[str] = list()
    for ws in workspaces:
        if display is not None and ws.get('output') != display:
            continue
        name = str(ws['name'])
        if _is_managed_workspace(name):
            names.append(name)
    return sorted(names, key=ws_sort_key)


def _is_managed_workspace(ws_name: str) -> bool:
    # Return True for normal named workspaces used by the helper.
    return bool(re.match(r'^[A-Z]+\d+$', ws_name)
                or re.match(r'^\d+$', ws_name))


def _is_native_ws(ws_name: str, letter: str) -> bool:
    # Return True if workspace belongs to the display with given letter.
    if re.match(rf'^{letter}\d+$', ws_name):
        return True  # Matches this display's letter prefix
    if re.match(r'^\d+$', ws_name):
        return True  # Default numeric workspace (e.g., sway's "1")
    return False


def _get_workspaces_data() -> list[dict[str, object]]:
    # Return the raw `get_workspaces` JSON payload as Python objects.
    data = json.loads(run_cmd('swaymsg -t get_workspaces'))
    if not isinstance(data, list):
        raise ValueError('swaymsg -t get_workspaces did not return a list')
    return data


def _get_focused_workspace_name(workspaces: list[dict[str, object]]) -> str:
    # Return the focused workspace name from get_workspaces payload.
    for ws in workspaces:
        if ws.get('focused'):
            return str(ws['name'])
    raise RuntimeError('No focused workspace found')


def _get_visible_workspace_name(workspaces: list[dict[str, object]],
                                display: str) -> str:
    # Return the visible workspace name for a specific display.
    for ws in workspaces:
        if ws.get('output') == display and ws.get('visible'):
            return str(ws['name'])
    for ws in workspaces:
        if ws.get('output') == display and ws.get('focused'):
            return str(ws['name'])
    raise RuntimeError(f'No visible workspace found for display: {display}')


def run_cmd(cmd: str) -> str:
    return subprocess.run(cmd, shell=True, check=True,
                          stdout=subprocess.PIPE).stdout.decode('utf-8')


def idx2char(idx: int) -> str:
    # Convert index to capital letter.
    return chr(65 + idx)


@contextlib.contextmanager
def _reorder_lock():  # type: ignore[return]
    # Exclusive file lock to serialise fix_workspace_order calls.
    fd = open(_REORDER_LOCK_FILE, 'w')
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
        fd.close()


@contextlib.contextmanager
def _move_lock():  # type: ignore[return]
    # Exclusive file lock to serialise move_nei_workspace calls.
    # Prevents a second keypress from reading focus state while the first
    # invocation's fix_workspace_order is temporarily shuffling focus.
    fd = open(_MOVE_LOCK_FILE, 'w')
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        fcntl.flock(fd, fcntl.LOCK_UN)
        fd.close()


def _strip_reorder_tmp_prefix(name: str) -> str:
    # Strip all reorder temp prefixes
    # (_WS_REORDER_TMP, _WS_SETUP_TMP, legacy _t).
    prev = None
    while prev != name:
        prev = name
        for prefix in (_WS_REORDER_TMP, _WS_SETUP_TMP, '_t'):
            if name.startswith(prefix):
                name = name[len(prefix):]
    return name


# -----------------------------------------------------------------------------
# -------------------------------- Display Map --------------------------------
# -----------------------------------------------------------------------------
def _update_display_map(all_disps: list[str]) -> dict[str, str]:
    # Load/create persistent output->letter map; add new displays.
    disp_to_letter = _load_display_map()
    used_letters = set(disp_to_letter.values())
    # Assign a letter to any newly seen display (in position-sorted order)
    for disp in all_disps:
        if disp not in disp_to_letter:
            for idx in range(26):
                letter = idx2char(idx)
                if letter not in used_letters:
                    disp_to_letter[disp] = letter
                    used_letters.add(letter)
                    break
    _save_display_map(disp_to_letter)
    return disp_to_letter


def _load_display_map() -> dict[str, str]:
    # Load display map from file; return {} on missing or invalid data.
    if not osp.exists(WS_DISP_MAP_FILE):
        return {}
    try:
        with open(WS_DISP_MAP_FILE, 'r') as f:
            data = json.load(f)
        if not isinstance(data, dict):
            return {}
        validated = {k: v for k, v in data.items()
                     if isinstance(k, str) and isinstance(v, str)
                     and re.match(r'^[A-Z]$', v)}
        # Reject map with duplicate letter assignments
        if len(validated.values()) != len(set(validated.values())):
            return {}
        return validated
    except Exception:
        return {}


def _save_display_map(disp_to_letter: dict[str, str]) -> None:
    # Save display map to persistent JSON file.
    with open(WS_DISP_MAP_FILE, 'w') as f:
        json.dump(disp_to_letter, f, indent=2)


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('action', type=str, help='Action to perform')
    parser.add_argument('--opt', type=str, help='Optional argument',
                        default='')
    argv = parser.parse_args()

    run_action(argv.action, argv.opt)


if __name__ == '__main__':
    main()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
