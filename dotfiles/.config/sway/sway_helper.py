import argparse
import subprocess
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
def run_action(action: str):
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
    focus_workspace(nxt_ws)


def move_nei_workspace(offset: int = 1):
    ''' Move to neighbor workspace on current display. '''
    cur_disp = get_cur_display()
    cur_ws = get_cur_workspace(cur_disp)
    nxt_ws = re.sub(r'\d+', lambda x: str((int(x.group()) + offset) % N_WS),
                    cur_ws)
    move_workspace(nxt_ws)


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


# -----------------------------------------------------------------------------
# ---------------------------- Sway Getter Wrappers ---------------------------
# -----------------------------------------------------------------------------
def get_displays() -> list:
    # Get display info with positions
    output_json = run_cmd("swaymsg -t get_outputs")
    outputs = json.loads(output_json)

    # Extract display names and their x positions
    display_positions = [(output['name'], output['rect']['x'])
                          for output in outputs if output['active']]

    # Sort by x position
    display_positions.sort(key=lambda x: x[1])

    # Return just the display names in sorted order
    all_disps = [name for name, _ in display_positions]
    return all_disps


def get_cur_display() -> str:
    return run_cmd("swaymsg -t get_outputs | jq -r '.[] | " +
                   "select(.focused) | .name'").strip()


def get_workspaces(display: Optional[str] = None) -> list:
    if display is None:
        # All workspaces
        return run_cmd("swaymsg -t get_workspaces | jq -r '.[] | " +
                       ".name'").splitlines()
    else:
        # Workspaces on a display
        return run_cmd("swaymsg -t get_workspaces | jq -r '.[] | " +
                       f"select(.output == \"{display}\") |" +
                       " .name'").splitlines()


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


# -----------------------------------------------------------------------------
# ---------------------------------- Utility ----------------------------------
# -----------------------------------------------------------------------------
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
    argv = parser.parse_args()

    # Run action
    run_action(argv.action)

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
