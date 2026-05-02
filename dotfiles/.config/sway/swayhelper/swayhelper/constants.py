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

# Fixed mapping from sway output name to workspace letter prefix.
# This mapping is absolute and does not change when displays are
# reconnected. Keys: sway output names (e.g. DP-2). Values: single
# uppercase letter used as workspace prefix (e.g. A).
OUTPUT_TO_LETTER: dict[str, str] = {
    'DP-2': 'A',
    'DP-1': 'B',
    'HDMI-A-2': 'C',
    'HDMI-A-1': 'D',
}

# Ordered list of output names for indexed display access via
# focus_display / move_display. Index 0 corresponds to OPT=0 (h key),
# 1 to OPT=1 (k), 2 to OPT=2 (j), 3 to OPT=3 (l).
INDEXED_OUTPUTS: list[str] = ['DP-2', 'DP-1', 'HDMI-A-2', 'HDMI-A-1']


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
