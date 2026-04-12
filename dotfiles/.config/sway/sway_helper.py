import argparse
import subprocess
import time
from typing import Optional
import re
import json


# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
N_WS = 10


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
    ''' Rename workspaces to A1, A2, B1, B2, etc. '''
    # Rename to temporal names
    for disp_idx, disp in enumerate(get_displays()):
        ws = get_workspaces(disp)
        for ws_idx, old_ws_name in enumerate(ws):
            new_ws_name = f'tmp_{disp_idx}{ws_idx}'
            rename_workspace(old_ws_name, new_ws_name)
    # Rename to desired names
    for disp_idx, disp in enumerate(get_displays()):
        ws = get_workspaces(disp)
        for ws_idx, old_ws_name in enumerate(ws):
            new_ws_name = f'{idx2char(disp_idx)}{ws_idx}'
            rename_workspace(old_ws_name, new_ws_name)


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


def run_cmd(cmd) -> str:
    return subprocess.run(cmd, shell=True, check=True,
                          stdout=subprocess.PIPE).stdout.decode("utf-8")


def idx2char(idx: int) -> str:
    ''' Convert index to capital letter. '''
    return chr(65 + idx)


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
