import enum
from typing import Callable, Optional

import i3ipc
import i3ipc.events

# -----------------------------------------------------------------------------
# --------------------------------- Constants ---------------------------------
# -----------------------------------------------------------------------------
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


DEFAULT_LAYOUT = LayoutKind.TALL


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
_IpcHandler = Callable[[i3ipc.Connection,
                        i3ipc.events.IpcBaseEvent], None]

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
        self.transforms: set[Transform] = (set()
                                           if transforms is None
                                           else set(transforms))

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
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
