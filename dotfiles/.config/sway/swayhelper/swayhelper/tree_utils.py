import shlex
from typing import Optional

import swayhelper.constants as _constants
from swayhelper.constants import _LAYOUT_BY_NAME, BindingEvent, Con, LayoutKind
from swayhelper.ipc import SwayConn
from swayhelper.state import WorkspaceState, _ws_states


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
                else _LAYOUT_BY_NAME.get(_constants.DEFAULT_LAYOUT.value,
                                         LayoutKind.TALL))
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
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
