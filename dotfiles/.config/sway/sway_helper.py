import argparse
import os
import os.path as osp
import subprocess
import time
from typing import Optional
import re
import json


# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
N_WS = 10
# Persistent file (per-user, survives sway reload but cleared on reboot)
WS_DISP_MAP_FILE = f'/tmp/sway_wsdisp_map_{os.getuid()}.json'


# -----------------------------------------------------------------------------
# ---------------------------- Action Entry Point -----------------------------
# -----------------------------------------------------------------------------
def run_action(action: str, opt: str = ''):
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
def setup_workspace_names():
    ''' Rename workspaces to A0, A1, B0, B1, etc. based on display map. '''
    all_disps = get_displays()
    # Ensure persistent display-letter map is up to date
    disp_to_letter = _update_display_map(all_disps)
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
            rename_workspace(old_name, f'__ws_{letter}{num}')
    # Pass 2: strip the tmp prefix to restore final names
    for disp in all_disps:
        letter = disp_to_letter[disp]
        prefix = f'__ws_{letter}'
        for ws_name in get_workspaces_raw(disp):
            if ws_name.startswith(prefix):
                rename_workspace(ws_name, ws_name[len('__ws_'):])


def _restore_workspace_assignments(all_disps: list[str],
                                   disp_to_letter: dict[str, str]):
    ''' Move workspaces with known letter prefixes to their home display. '''
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


def focus_nei_workspace(offset: int = 1):
    ''' Focus to neighbor workspace on current display. '''
    cur_disp = get_cur_display()
    cur_ws = get_cur_workspace(cur_disp)
    nxt_ws = re.sub(r'\d+', lambda x: str((int(x.group()) + offset) % N_WS),
                    cur_ws)
    # Record existing workspaces before creation
    ws_before = set(get_workspaces_raw(cur_disp))
    focus_workspace(nxt_ws)
    # Fix sway's internal order if a new workspace was created
    if nxt_ws not in ws_before:
        fix_workspace_order(cur_disp, nxt_ws)


def move_nei_workspace(offset: int = 1):
    ''' Move to neighbor workspace on current display. '''
    cur_disp = get_cur_display()
    cur_ws = get_cur_workspace(cur_disp)
    nxt_ws = re.sub(r'\d+', lambda x: str((int(x.group()) + offset) % N_WS),
                    cur_ws)
    # Record existing workspaces before creation
    ws_before = set(get_workspaces_raw(cur_disp))
    move_workspace(nxt_ws)
    # Fix sway's internal order if a new workspace was created
    if nxt_ws not in ws_before:
        fix_workspace_order(cur_disp, nxt_ws)


def fix_workspace_order(display: str, new_ws: str):
    ''' Reorder sway's workspace list after a new one is created mid-sequence.

    When B1 is created after B0,B2, sway appends it: B0,B2,B1.
    This evacuates B2 to a temp WS (sway auto-deletes the empty B2),
    then recreates it in sorted position: B0,B1,B2. '''
    ws_actual = get_workspaces_raw(display)
    if new_ws not in ws_actual:
        return
    new_ws_pos = ws_actual.index(new_ws)

    # Workspaces placed before new_ws in sway's list that should come after it
    ws_to_reorder = [ws for ws in ws_actual[:new_ws_pos]
                     if ws_sort_key(ws) > ws_sort_key(new_ws)]
    if not ws_to_reorder:
        return

    # Evacuate each out-of-order workspace's windows to a temp workspace
    saved_windows: dict[str, list] = {}
    for ws in ws_to_reorder:
        saved_windows[ws] = get_windows_on_workspace(ws)
        for win_id in saved_windows[ws]:
            move_window_to_workspace(win_id, f'_t{ws}')
    # Poll until out-of-order workspaces are auto-deleted (usually instant)
    for _ in range(15):
        if not (set(get_workspaces_raw(display)) & set(ws_to_reorder)):
            break
        time.sleep(0.02)

    # Recreate workspaces in sorted order (focusing an unknown WS creates it)
    for ws in sorted(ws_to_reorder, key=ws_sort_key):
        focus_workspace(ws)
    # Return focus to the newly created workspace
    focus_workspace(new_ws)

    # Restore windows to their correct workspaces
    for ws in ws_to_reorder:
        for win_id in saved_windows[ws]:
            move_window_to_workspace(win_id, ws)


def focus_valid_nei_workspace(offset: int = 1):
    ''' Focus to valid neighbor workspace on current display. '''
    cur_disp = get_cur_display()
    ws = get_workspaces(cur_disp)
    cur_ws = get_cur_workspace(cur_disp)
    nxt_ws = ws[(ws.index(cur_ws) + offset) % len(ws)]
    focus_workspace(nxt_ws)


def focus_nei_display(offset: int = 1):
    ''' Focus to neighbor display. '''
    all_disps = get_displays()
    cur_disp = get_cur_display()
    nxt_disp = all_disps[(all_disps.index(cur_disp) + offset) % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    focus_workspace(nxt_ws)


def move_nei_display(offset: int = 1):
    ''' Move to neighbor display. '''
    all_disps = get_displays()
    cur_disp = get_cur_display()
    nxt_disp = all_disps[(all_disps.index(cur_disp) + offset) % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    move_workspace(nxt_ws)


def focus_display(index: int):
    ''' Focus to display by index. '''
    all_disps = get_displays()
    nxt_disp = all_disps[index % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    focus_workspace(nxt_ws)


def move_display(index: int):
    ''' Move to display by index. '''
    all_disps = get_displays()
    nxt_disp = all_disps[index % len(all_disps)]
    nxt_ws = get_cur_workspace(nxt_disp)
    move_workspace(nxt_ws)


# -----------------------------------------------------------------------------
# ---------------------------- Sway Getter Wrappers ---------------------------
# -----------------------------------------------------------------------------
def get_displays() -> list:
    # Get display info with positions
    output_json = run_cmd("swaymsg -t get_outputs")
    outputs = json.loads(output_json)

    # Extract display names and their x positions
    display_positions = [(output['name'],
                          output['rect']['x'],
                          output['rect']['y'])
                         for output in outputs if output['active']]

    # Sort by x position
    display_positions.sort(key=lambda x: (x[1], x[2]))

    # Return just the display names in sorted order
    all_disps = [name for name, _, _ in display_positions]
    return all_disps


def get_cur_display() -> str:
    return run_cmd("swaymsg -t get_outputs | jq -r '.[] | " +
                   "select(.focused) | .name'").strip()


def get_workspaces(display: Optional[str] = None) -> list:
    ''' Get workspaces sorted by name. '''
    return sorted(get_workspaces_raw(display), key=ws_sort_key)


def get_workspaces_raw(display: Optional[str] = None) -> list:
    ''' Get workspaces in sway's natural (creation) order. '''
    if display is None:
        return run_cmd("swaymsg -t get_workspaces | jq -r '.[] | "
                       ".name'").splitlines()
    else:
        return run_cmd("swaymsg -t get_workspaces | jq -r '.[] | "
                       f"select(.output == \"{display}\") | "
                       ".name'").splitlines()


def _get_all_workspaces_with_output() -> list[tuple[str, str]]:
    ''' Return (workspace_name, output_name) pairs for all workspaces. '''
    workspaces = json.loads(run_cmd('swaymsg -t get_workspaces'))
    return [(ws['name'], ws['output']) for ws in workspaces]


def get_windows_on_workspace(ws_name: str) -> list:
    ''' Get all window container IDs on a workspace (recursive). '''
    tree = json.loads(run_cmd('swaymsg -t get_tree'))
    ws_node = _find_node(tree,
                         lambda n: n.get('type') == 'workspace'
                         and n.get('name') == ws_name)
    if ws_node is None:
        return []
    return _collect_windows(ws_node)


def _find_node(node: dict, predicate) -> Optional[dict]:
    ''' Find first node matching predicate in sway tree (DFS). '''
    if predicate(node):
        return node
    children = node.get('nodes', []) + node.get('floating_nodes', [])
    for child in children:
        result = _find_node(child, predicate)
        if result is not None:
            return result
    return None


def _collect_windows(node: dict) -> list:
    ''' Collect all leaf window con_ids under a node. '''
    children = node.get('nodes', []) + node.get('floating_nodes', [])
    if not children:
        return [node['id']] if node.get('type') == 'con' else []
    result = []
    for child in children:
        result.extend(_collect_windows(child))
    return result


def get_cur_workspace(display: Optional[str] = None) -> str:
    if display is None:
        # Current display's active workspace
        return run_cmd("swaymsg -t get_workspaces | jq -r '.[] | " +
                       "select(.focused) | .name'").strip()
    else:
        # Showing workspace on a display
        return run_cmd("swaymsg -t get_outputs | jq -r '.[] | " +
                       f"select(.name == \"{display}\") | " +
                       ".current_workspace'").strip()


# -----------------------------------------------------------------------------
# ---------------------------- Sway Action Wrappers ---------------------------
# -----------------------------------------------------------------------------
def rename_workspace(old_name: str, new_name: str):
    run_cmd(f"swaymsg 'rename workspace {old_name} to {new_name}'")


def focus_workspace(ws: str):
    run_cmd(f"swaymsg 'workspace {ws}'")


def move_workspace(ws: str):
    run_cmd(f"swaymsg 'move container to workspace {ws}'")  # Move
    focus_workspace(ws)  # Focus


def move_window_to_workspace(win_id: int, ws_name: str):
    ''' Move a specific window by con_id to a target workspace. '''
    run_cmd(f"swaymsg '[con_id={win_id}] move to workspace {ws_name}'")


# -----------------------------------------------------------------------------
# ---------------------------------- Utility ----------------------------------
# -----------------------------------------------------------------------------
def ws_sort_key(name: str) -> tuple:
    ''' Sort key for workspace names like A0, B1, C2. '''
    match = re.match(r'([A-Z]*)(\d+)', name)
    if match:
        return (match.group(1), int(match.group(2)))
    return (name, 0)


def _is_native_ws(ws_name: str, letter: str) -> bool:
    ''' Return True if workspace belongs to the display with given letter. '''
    if re.match(rf'^{letter}\d+$', ws_name):
        return True  # Matches this display's letter prefix
    if re.match(r'^\d+$', ws_name):
        return True  # Default numeric workspace (e.g., sway's "1")
    return False


def run_cmd(cmd) -> str:
    return subprocess.run(cmd, shell=True, check=True,
                          stdout=subprocess.PIPE).stdout.decode("utf-8")


def idx2char(idx: int) -> str:
    ''' Convert index to capital letter. '''
    return chr(65 + idx)


# -----------------------------------------------------------------------------
# -------------------------------- Display Map --------------------------------
# -----------------------------------------------------------------------------
def _update_display_map(all_disps: list[str]) -> dict[str, str]:
    ''' Load/create persistent output->letter map; add new displays. '''
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
    ''' Load display map from file; return {} on missing or invalid data. '''
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


def _save_display_map(disp_to_letter: dict[str, str]):
    ''' Save display map to persistent JSON file. '''
    with open(WS_DISP_MAP_FILE, 'w') as f:
        json.dump(disp_to_letter, f, indent=2)


# -----------------------------------------------------------------------------
# -------------------------------- Entry Point --------------------------------
# -----------------------------------------------------------------------------
if __name__ == '__main__':
    # Arguments
    parser = argparse.ArgumentParser()
    parser.add_argument('action', type=str, help='Action to perform')
    parser.add_argument('--opt', type=str, help='Optional argument',
                        default='')
    argv = parser.parse_args()

    # Run action
    run_action(argv.action, argv.opt)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
