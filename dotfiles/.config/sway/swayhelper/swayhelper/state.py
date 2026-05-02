from typing import Optional

import swayhelper.constants as _constants
from swayhelper.constants import LayoutKind, Transform


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
        return _constants.LAYOUT_COLS.get(self.kind, 1)


# Per-workspace layout states, keyed by workspace con_id
ws_states: dict[int, WorkspaceState] = dict()

# Pending daemon-initiated moves: container_id -> expiry time (monotonic).
# The resulting WINDOW_MOVE event is suppressed to prevent infinite reflow.
# Entries expire after _constants.MOVE_ID_TTL seconds so a sway-dropped
# event never permanently suppresses future user moves on the same container.
daemon_move_ids: dict[int, float] = dict()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
