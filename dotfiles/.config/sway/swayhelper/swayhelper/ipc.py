import i3ipc

from swayhelper.constants import Con


# -----------------------------------------------------------------------------
# ------------------------------ IPC Connection -------------------------------
# -----------------------------------------------------------------------------
class SwayConn(i3ipc.Connection):
    # Sway IPC connection with batched command dispatch.
    # While buffering is active, command() calls are accumulated locally.
    # On flush(), all accumulated commands are sent as a single IPC
    # round-trip, reducing latency during multi-step layout operations.

    def __init__(self) -> None:
        super().__init__()
        self._buffering = False
        self._buf: list[str] = list()

    def command(self, payload: str) -> list:  # type: ignore[override]
        if self._buffering:
            self._buf.append(payload)
            return []
        return super().command(payload)

    def start_buffering(self) -> None:
        # Begin accumulating subsequent command() calls.
        self._buffering = True

    def flush(self) -> list:
        # Send all buffered commands in one IPC call; disable buffering.
        self._buffering = False
        if not self._buf:
            return []
        payload = ';'.join(self._buf)
        self._buf.clear()
        return super().command(payload)

    def discard(self) -> None:
        # Discard buffered commands without sending; disable buffering.
        self._buffering = False
        self._buf.clear()

    def get_tree(self) -> Con:  # type: ignore[override]
        # Flush pending moves before reading tree to get a consistent view
        was_buffering = self._buffering
        self.flush()
        try:
            tree = super().get_tree()
        finally:
            if was_buffering:
                self.start_buffering()
        return tree  # type: ignore[return-value]

    def get_workspaces(self) -> list:  # type: ignore[override]
        was_buffering = self._buffering
        self.flush()
        try:
            workspaces = super().get_workspaces()
        finally:
            if was_buffering:
                self.start_buffering()
        return workspaces


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
