import enum
from typing import Callable

import i3ipc
import i3ipc.events

# Package-wide constants shared between daemon and helper modules.

# Temporary sway mark used to teleport containers during layout moves
MOVE_MARK = '__tiler_mark'

# Workspace name prefixes used by helper.py for temporary rename operations.
# These workspaces must be skipped during layout to avoid tiling zombies.
TEMP_WS_PREFIXES = ('__ws_', '__swh_tmp_')

# Maximum reflow iterations per window; scales the convergence guard in
# _run_ncol_layout to handle large workspaces without false timeouts.
MAX_REFLOW_ITERS_PER_WIN = 3

# Time-to-live for daemon-move suppression entries (seconds).
# Generous bound; sway events typically arrive within milliseconds.
MOVE_ID_TTL = 2.0


# -----------------------------------------------------------------------------
# ----------------------------------- Enums -----------------------------------
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
LAYOUT_COLS: dict[LayoutKind, int] = {
    LayoutKind.TALL: 2,
    LayoutKind.STACK: 1,
    LayoutKind.THREE_COL: 3,
}

# String-to-enum map for use with the 'set_layout' command
LAYOUT_BY_NAME: dict[str, LayoutKind] = {k.value: k for k in LayoutKind}

# i3ipc type aliases; Con attrs are set dynamically via setattr, so alias to
# Any to avoid false positives from pyright when stubs are not present.
Con = object
WindowEvent = i3ipc.events.WindowEvent
BindingEvent = i3ipc.events.BindingEvent

# Callable type for nop command handlers (i3, event, *args)
CmdHandler = Callable[..., None]

# Callable type matching i3ipc.Connection.on() handler signature
IpcHandler = Callable[[i3ipc.Connection,
                       i3ipc.events.IpcBaseEvent], None]

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
