from types import SimpleNamespace
from typing import Any, Optional, cast

from swayhelper import daemon


def test_run_existing_layouts_retiles_all_workspaces(monkeypatch) -> None:
    calls: list[int] = []

    class FakeConn:
        def get_workspaces(self) -> list[SimpleNamespace]:
            return [
                SimpleNamespace(ipc_data={'id': 11}),
                SimpleNamespace(ipc_data={'id': 22}),
            ]

    def fake_run_layout(_i3: object, ws_id: int) -> None:
        calls.append(ws_id)

    monkeypatch.setattr(daemon, '_run_layout', fake_run_layout)

    daemon._run_existing_layouts(cast(daemon.SwayConn, FakeConn()))

    assert calls == [11, 22]


def test_on_window_skips_layout_generated_move(monkeypatch) -> None:
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._move_count[0] = 1
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='move'))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert daemon._move_count[0] == 0
    assert actions == []


def test_on_window_retiles_after_close(monkeypatch) -> None:
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._move_count[0] = 0
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='close'))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert actions == ['start', 'run', 'flush']


def test_on_window_new_swaps_before_reflow(monkeypatch) -> None:
    '''New window is inserted before its predecessor before layout reflow.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._move_count[0] = 0
    monkeypatch.setattr(daemon, '_swap_new_before_prev',
                        lambda _i3, _id: actions.append('swap'))
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='new',
                                      container=SimpleNamespace(id=99)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert actions == ['start', 'swap', 'run', 'flush']


def test_swap_new_before_prev_swaps_with_predecessor(monkeypatch) -> None:
    '''_swap_new_before_prev swaps new window with the leaf just before it.'''
    NEW_ID = 30
    PREV_ID = 20
    AFTER_ID = 40

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)

    prev_leaf = FakeLeaf(PREV_ID)
    new_leaf = FakeLeaf(NEW_ID)
    after_leaf = FakeLeaf(AFTER_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            if con_id == NEW_ID:
                return new_leaf
            if con_id == PREV_ID:
                return prev_leaf
            return None

    # prev, new, after — new is in the middle, so swap should happen
    class FakeWsCon:
        id = 1

        def leaves(self) -> list[FakeLeaf]:
            return [prev_leaf, new_leaf, after_leaf]

    new_leaf.workspace = lambda: FakeWsCon()  # type: ignore[method-assign]

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_new_before_prev(cast(daemon.SwayConn, FakeConn()), NEW_ID)

    assert f'swap container with con_id {PREV_ID}' in new_leaf.commands
    assert 'focus' in new_leaf.commands


def test_swap_new_before_prev_skips_when_predecessor_is_last(
        monkeypatch) -> None:
    '''_swap_new_before_prev does nothing when the predecessor was last.'''
    NEW_ID = 30
    PREV_ID = 20

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)

    prev_leaf = FakeLeaf(PREV_ID)
    new_leaf = FakeLeaf(NEW_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            if con_id == NEW_ID:
                return new_leaf
            return None

    # prev, new — new is last; predecessor was last, so no swap
    class FakeWsCon:
        id = 1

        def leaves(self) -> list[FakeLeaf]:
            return [prev_leaf, new_leaf]

    new_leaf.workspace = lambda: FakeWsCon()  # type: ignore[method-assign]

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_new_before_prev(cast(daemon.SwayConn, FakeConn()), NEW_ID)

    assert new_leaf.commands == []


def test_swap_new_before_prev_skips_when_first(monkeypatch) -> None:
    '''_swap_new_before_prev does nothing when new window is already first.'''
    NEW_ID = 10

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)

    new_leaf = FakeLeaf(NEW_ID)

    class FakeWsCon:
        id = 1

        def leaves(self) -> list[FakeLeaf]:
            return [new_leaf]

    new_leaf.workspace = lambda: FakeWsCon()  # type: ignore[method-assign]

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return new_leaf if con_id == NEW_ID else None

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_new_before_prev(cast(daemon.SwayConn, FakeConn()), NEW_ID)

    assert new_leaf.commands == []


def test_swap_new_before_prev_skips_floating(monkeypatch) -> None:
    '''_swap_new_before_prev does nothing for floating new windows.'''
    NEW_ID = 42

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_on'
            self.type = 'con'
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)

    new_leaf = FakeLeaf(NEW_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return new_leaf if con_id == NEW_ID else None

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_new_before_prev(cast(daemon.SwayConn, FakeConn()), NEW_ID)

    assert new_leaf.commands == []


def test_reflow_ncol_expands_single_column(monkeypatch) -> None:
    actions: list[str] = []

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)
            actions.append(payload)

    class FakeColumn:
        def __init__(self, nodes: list[FakeLeaf]) -> None:
            self.nodes = nodes
            self.layout = 'splitv'

        def command(self, payload: str) -> None:
            actions.append(payload)

    class FakeWorkspace:
        def __init__(self, col: FakeColumn, focused: FakeLeaf) -> None:
            self.nodes = [col]
            self.layout = 'splith'
            self._focused = focused

        def leaves(self) -> list[FakeLeaf]:
            return list(self.nodes[0].nodes)

        def find_focused(self) -> FakeLeaf:
            return self._focused

    master = FakeLeaf(1)
    slave = FakeLeaf(2)
    col = FakeColumn([master, slave])
    ws = FakeWorkspace(col, master)
    state = daemon.WorkspaceState(ws_id=1)

    monkeypatch.setattr(daemon, '_refetch', lambda _i3, con: con)

    assert daemon._reflow_ncol(cast(daemon.SwayConn, object()), state, ws)
    assert slave.commands == ['move right']
    assert master.commands == ['focus']


def test_get_resize_target_uses_master_pane_not_biggest_window() -> None:
    class FakeLeaf:
        def __init__(self, con_id: int, area: int = 1) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'
            self.rect = SimpleNamespace(width=area, height=1)

        def leaves(self) -> list['FakeLeaf']:
            return [self]

    class FakeColumn:
        def __init__(self, nodes: list[FakeLeaf]) -> None:
            self.nodes = nodes

        def leaves(self) -> list[FakeLeaf]:
            return list(self.nodes)

    class FakeWorkspace:
        def __init__(self, nodes: list[FakeColumn], focused: FakeLeaf) -> None:
            self.nodes = nodes
            self.layout = 'splith'
            self._focused = focused

        def leaves(self) -> list[FakeLeaf]:
            leaves: list[FakeLeaf] = []
            for node in self.nodes:
                leaves.extend(node.leaves())
            return leaves

        def find_focused(self) -> FakeLeaf:
            return self._focused

    master = FakeLeaf(1, area=20)
    slave = FakeLeaf(2, area=100)
    ws = FakeWorkspace([FakeColumn([master]), FakeColumn([slave])], slave)
    state = daemon.WorkspaceState(ws_id=1)

    target = daemon._get_resize_target(ws, state)
    assert target is not None
    assert target.id == master.id


def test_run_ncol_layout_resets_state_on_single_window() -> None:
    class FakeLeaf:
        floating = 'user_off'
        type = 'con'

    class FakeWorkspace:
        def leaves(self) -> list[FakeLeaf]:
            return [FakeLeaf()]

    class FakeTree:
        def find_by_id(self, _ws_id: int) -> FakeWorkspace:
            return FakeWorkspace()

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    state = daemon.WorkspaceState(
        ws_id=99, kind=daemon.LayoutKind.THREE_COL,
        n_masters=2, transforms={daemon.Transform.REFLECTX})

    daemon._run_ncol_layout(cast(daemon.SwayConn, FakeConn()), state)

    assert state.kind == daemon._LAYOUT_BY_NAME.get(daemon.DEFAULT_LAYOUT)
    assert state.n_masters == 1
    assert state.transforms == set()


def test_run_nop_layout_resets_state_on_single_window() -> None:
    class FakeLeaf:
        floating = 'user_off'
        type = 'con'

    class FakeWorkspace:
        def leaves(self) -> list[FakeLeaf]:
            return [FakeLeaf()]

    class FakeTree:
        def find_by_id(self, _ws_id: int) -> FakeWorkspace:
            return FakeWorkspace()

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    state = daemon.WorkspaceState(
        ws_id=99, kind=daemon.LayoutKind.NOP,
        n_masters=3, transforms={daemon.Transform.TRANSPOSE})

    daemon._run_nop_layout(cast(daemon.SwayConn, FakeConn()), state)

    assert state.kind == daemon._LAYOUT_BY_NAME.get(daemon.DEFAULT_LAYOUT)
    assert state.n_masters == 1
    assert state.transforms == set()


def test_get_master_window_respects_reflectx_order() -> None:
    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'

        def leaves(self) -> list['FakeLeaf']:
            return [self]

    class FakeColumn:
        def __init__(self, node: FakeLeaf) -> None:
            self.nodes = [node]

        def leaves(self) -> list[FakeLeaf]:
            return list(self.nodes)

    class FakeWorkspace:
        def __init__(self, nodes: list[FakeColumn]) -> None:
            self.nodes = nodes
            self.layout = 'splith'

        def leaves(self) -> list[FakeLeaf]:
            leaves: list[FakeLeaf] = []
            for node in self.nodes:
                leaves.extend(node.leaves())
            return leaves

        def find_focused(self) -> None:
            return None

    left = FakeLeaf(1)
    right = FakeLeaf(2)
    ws = FakeWorkspace([FakeColumn(left), FakeColumn(right)])
    state = daemon.WorkspaceState(
        ws_id=1, transforms={daemon.Transform.REFLECTX})

    master = daemon._get_master_window(ws, state)
    assert master is not None
    assert master.id == right.id


def test_run_ncol_layout_restores_pre_reflow_focus(monkeypatch) -> None:
    # After _reflow_ncol moves windows around, sway's internal focus may
    # drift to a different window (e.g. the master). _run_ncol_layout must
    # restore focus to the window that was focused BEFORE the reflow, not
    # whatever sway happens to have focused after the layout moves.
    refocused_ids: list[int] = []

    MASTER_ID = 1
    MOVED_WIN_ID = 42
    WS_ID = 99

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.fullscreen_mode = 0
            self.floating = 'user_off'
            self.type = 'con'

        def command(self, _payload: str) -> None:
            pass

        def leaves(self) -> list['FakeLeaf']:
            return [self]

    master_leaf = FakeLeaf(MASTER_ID)
    moved_leaf = FakeLeaf(MOVED_WIN_ID)

    class FakeWorkspace:
        def __init__(self, focused_leaf: FakeLeaf) -> None:
            self.id = WS_ID
            self.layout = 'splith'
            self._focused = focused_leaf

        def leaves(self) -> list[FakeLeaf]:
            return [master_leaf, moved_leaf]

        def find_focused(self) -> FakeLeaf:
            return self._focused

    # Initial state: moved window is focused (set by atomic swaymsg in helper)
    ws_before_reflow = FakeWorkspace(focused_leaf=moved_leaf)
    # After reflow: sway shifted focus to master due to layout move commands
    ws_after_reflow = FakeWorkspace(focused_leaf=master_leaf)

    state = daemon.WorkspaceState(ws_id=WS_ID)

    monkeypatch.setattr(daemon, '_refetch',
                        lambda _i3, _con: ws_after_reflow)
    monkeypatch.setattr(daemon, '_reflow_ncol',
                        lambda _i3, _state, _ws: False)
    monkeypatch.setattr(daemon, '_get_focused_workspace',
                        lambda _i3: ws_after_reflow)
    monkeypatch.setattr(daemon, '_refocus_window',
                        lambda _i3, win: refocused_ids.append(win.id))

    # Track calls to get_tree().find_by_id: first call (ws lookup) returns
    # ws_before_reflow so that ws.find_focused() yields moved_leaf;
    # subsequent find_by_id(MOVED_WIN_ID) calls for focus-restoration
    # return moved_leaf directly.
    ws_lookup_done = [False]

    class FakeTree2:
        def find_by_id(self, con_id: int) -> Any:
            if con_id == WS_ID and not ws_lookup_done[0]:
                ws_lookup_done[0] = True
                return ws_before_reflow
            if con_id == MOVED_WIN_ID:
                return moved_leaf
            return ws_after_reflow

    class FakeConn2:
        def get_tree(self) -> FakeTree2:
            return FakeTree2()

        def get_workspaces(self) -> list[SimpleNamespace]:
            return [SimpleNamespace(ipc_data={'id': WS_ID}, focused=True)]

    daemon._run_ncol_layout(cast(daemon.SwayConn, FakeConn2()), state)

    assert refocused_ids == [MOVED_WIN_ID], (
        f'Expected focus on moved window {MOVED_WIN_ID}, got {refocused_ids}')
