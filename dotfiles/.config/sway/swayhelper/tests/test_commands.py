from typing import Optional, cast

from swayhelper import commands as cmd_mod
from swayhelper.state import WorkspaceState


# -----------------------------------------------------------------------------
# ------------------------------ Fake containers ------------------------------
# -----------------------------------------------------------------------------
class FakeLeaf:
    def __init__(self, con_id: int, floating: str = 'user_off') -> None:
        self.id = con_id
        self.floating = floating
        self.type = 'con'
        self.commands: list[str] = list()

    def command(self, payload: str) -> None:
        self.commands.append(payload)

    def leaves(self) -> list['FakeLeaf']:
        return [self]


class FakeWs:
    def __init__(self, ws_id: int, leaves: list[FakeLeaf],
                 focused: Optional[FakeLeaf] = None) -> None:
        self.id = ws_id
        self._leaves = leaves
        self._focused = focused

    def leaves(self) -> list[FakeLeaf]:
        return list(self._leaves)

    def find_focused(self) -> Optional[FakeLeaf]:
        return self._focused


# -----------------------------------------------------------------------------
# ---------------------------- _cmd_promote_window ----------------------------
# -----------------------------------------------------------------------------
def test_promote_window_inserts_before_master(monkeypatch) -> None:
    # Focused is behind master; should bubble up to master position.
    master = FakeLeaf(1)
    win2 = FakeLeaf(2)
    focused = FakeLeaf(3)
    win4 = FakeLeaf(4)
    ws = FakeWs(10, [master, win2, focused, win4], focused=focused)

    monkeypatch.setattr(cmd_mod, '_get_focused_workspace', lambda _i3: ws)
    monkeypatch.setattr(cmd_mod, '_get_focused_window', lambda _i3: focused)
    monkeypatch.setattr(cmd_mod, '_is_floating',
                        lambda c: c.floating != 'user_off')
    monkeypatch.setattr(cmd_mod, '_get_ws_state',
                        lambda _ws_id:     WorkspaceState(ws_id=10))

    # Master window is the first leaf
    monkeypatch.setattr(cmd_mod, '_get_master_window',
                        lambda _ws, _state: master)

    cmd_mod._cmd_promote_window(
        cast(cmd_mod.SwayConn, object()), object())  # type: ignore[arg-type]

    assert focused.commands == [
        'swap container with con_id 2',
        'swap container with con_id 1',
        'focus',
    ]


def test_promote_window_noop_when_already_master(monkeypatch) -> None:
    master = FakeLeaf(1)
    ws = FakeWs(10, [master], focused=master)

    monkeypatch.setattr(cmd_mod, '_get_focused_workspace', lambda _i3: ws)
    monkeypatch.setattr(cmd_mod, '_get_focused_window', lambda _i3: master)
    monkeypatch.setattr(cmd_mod, '_is_floating', lambda c: False)
    monkeypatch.setattr(cmd_mod, '_get_ws_state',
                        lambda _ws_id:     WorkspaceState(ws_id=10))
    monkeypatch.setattr(cmd_mod, '_get_master_window',
                        lambda _ws, _state: master)

    cmd_mod._cmd_promote_window(
        cast(cmd_mod.SwayConn, object()), object())  # type: ignore[arg-type]

    assert master.commands == list()


def test_promote_window_noop_when_floating(monkeypatch) -> None:
    focused = FakeLeaf(5, floating='user_on')
    master = FakeLeaf(1)
    ws = FakeWs(10, [master, focused], focused=focused)

    monkeypatch.setattr(cmd_mod, '_get_focused_workspace', lambda _i3: ws)
    monkeypatch.setattr(cmd_mod, '_get_focused_window', lambda _i3: focused)
    monkeypatch.setattr(cmd_mod, '_is_floating',
                        lambda c: c.floating in ('user_on', 'auto_on'))
    monkeypatch.setattr(cmd_mod, '_get_ws_state',
                        lambda _ws_id:     WorkspaceState(ws_id=10))
    monkeypatch.setattr(cmd_mod, '_get_master_window',
                        lambda _ws, _state: master)

    cmd_mod._cmd_promote_window(
        cast(cmd_mod.SwayConn, object()), object())  # type: ignore[arg-type]

    assert focused.commands == list()


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------