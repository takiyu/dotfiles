from types import SimpleNamespace
from typing import Any, Optional, cast

from swayhelper import daemon


def test_run_existing_layouts_retiles_all_workspaces(monkeypatch) -> None:
    calls: list[int] = []

    class FakeConn:
        def get_workspaces(self) -> list[SimpleNamespace]:
            return [
                SimpleNamespace(ipc_data={'id': 11}, focused=False, name='A0'),
                SimpleNamespace(ipc_data={'id': 22}, focused=False, name='B0'),
            ]

    def fake_run_layout(_i3: object, ws_id: int,
                        _focused_ws_id: Optional[int] = None) -> None:
        calls.append(ws_id)

    monkeypatch.setattr(daemon, '_run_layout', fake_run_layout)

    daemon._run_existing_layouts(cast(daemon.SwayConn, FakeConn()))

    assert calls == [11, 22]


def test_run_existing_layouts_skips_temp_workspaces(monkeypatch) -> None:
    '''Workspaces with temp helper prefixes must not be laid out.'''
    calls: list[int] = []

    class FakeTempConn:
        def get_workspaces(self) -> list[SimpleNamespace]:
            return [
                SimpleNamespace(ipc_data={'id': 10}, focused=False, name='A0'),
                SimpleNamespace(ipc_data={'id': 20}, focused=False,
                                name='__ws_B0'),
                SimpleNamespace(ipc_data={'id': 30}, focused=False,
                                name='__swh_tmp_A1'),
            ]

    def fake_run_layout(_i3: object, ws_id: int,
                        _focused_ws_id: Optional[int] = None) -> None:
        calls.append(ws_id)

    monkeypatch.setattr(daemon, '_run_layout', fake_run_layout)

    daemon._run_existing_layouts(cast(daemon.SwayConn, FakeTempConn()))

    # Only the real workspace A0 must be laid out; temp workspaces skipped
    assert calls == [10]


def test_on_window_skips_layout_generated_move(monkeypatch) -> None:
    '''Daemon-initiated move (container in _daemon_move_ids) is suppressed.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    daemon._daemon_move_ids[42] = float('inf')  # non-expiring entry
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='move',
                                      container=SimpleNamespace(id=42)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert 42 not in daemon._daemon_move_ids  # entry consumed
    assert actions == []  # no buffering/reflow/flush


def test_on_window_retiles_after_close(monkeypatch) -> None:
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='close',
                                      container=SimpleNamespace(id=55)))
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

    daemon._daemon_move_ids.clear()
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

    assert daemon._reflow_ncol(cast(daemon.SwayConn, object()), state, ws,
                               is_focused=True)
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


def test_run_ncol_layout_preserves_state_on_single_window() -> None:
    # Layout state (kind/n_masters/transforms) must survive a single-window
    # workspace; resetting here would clear custom layouts on other displays
    # whenever a window event fires globally via _run_existing_layouts.
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

    assert state.kind == daemon.LayoutKind.THREE_COL
    assert state.n_masters == 2
    assert state.transforms == {daemon.Transform.REFLECTX}


def test_run_nop_layout_preserves_state_on_single_window() -> None:
    # Layout state must survive a single-window workspace; same cross-display
    # reset hazard as in the ncol case above.
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

    assert state.kind == daemon.LayoutKind.NOP
    assert state.n_masters == 3
    assert state.transforms == {daemon.Transform.TRANSPOSE}


def test_run_nop_layout_no_focus_steal_on_non_focused_ws() -> None:
    # Non-focused NOP workspaces must not call focused.command('focus');
    # focused NOP workspaces must still restore focus as expected.
    focus_commands: list[int] = []

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'

        def command(self, payload: str) -> None:
            if payload == 'focus':
                focus_commands.append(self.id)

        def leaves(self) -> list['FakeLeaf']:
            return [self]

    leaf1 = FakeLeaf(1)
    leaf2 = FakeLeaf(2)

    class FakeWorkspace:
        id = 99

        def leaves(self) -> list[FakeLeaf]:
            return [leaf1, leaf2]

        def find_focused(self) -> FakeLeaf:
            return leaf1

    class FakeTree:
        def find_by_id(self, _con_id: int) -> FakeWorkspace:
            return FakeWorkspace()

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    state = daemon.WorkspaceState(ws_id=99, kind=daemon.LayoutKind.NOP)

    # Non-focused case: focused_ws_id belongs to a different workspace
    daemon._run_nop_layout(cast(daemon.SwayConn, FakeConn()), state,
                           focused_ws_id=55)
    assert focus_commands == [], (
        'Non-focused NOP workspace must not steal focus')

    # Focused case: focused_ws_id matches this workspace
    daemon._run_nop_layout(cast(daemon.SwayConn, FakeConn()), state,
                           focused_ws_id=99)
    assert focus_commands == [leaf1.id], (
        'Focused NOP workspace should restore focus')


def test_run_existing_layouts_no_focus_steal_on_non_focused_ws(
        monkeypatch) -> None:
    # Regression: when _run_existing_layouts processes all workspaces after
    # a window move, the non-focused workspace (source) must pass
    # is_focused=False to _reflow_ncol, while the focused workspace
    # (destination) passes is_focused=True.
    WS_A_ID = 10  # source workspace (non-focused)
    WS_B_ID = 20  # destination workspace (focused)

    reflow_calls: list[tuple[int, bool]] = []

    class FakeLeaf:
        floating = 'user_off'
        type = 'con'

    class FakeWs:
        def __init__(self, ws_id: int) -> None:
            self.id = ws_id
            self.layout = 'splith'

        def leaves(self) -> list[FakeLeaf]:
            return [FakeLeaf(), FakeLeaf()]

        def find_focused(self) -> None:
            return None

        def workspace(self) -> 'FakeWs':
            return self

    ws_a = FakeWs(WS_A_ID)
    ws_b = FakeWs(WS_B_ID)

    def fake_reflow(_i3: Any, _state: Any, ws: Any,
                    is_focused: bool = False) -> bool:
        reflow_calls.append((ws.id, is_focused))
        return False

    class FakeTree:
        def find_by_id(self, con_id: int) -> object:
            return ws_a if con_id == WS_A_ID else ws_b

    class FakeConn:
        def get_workspaces(self) -> list:
            from types import SimpleNamespace
            return [
                SimpleNamespace(ipc_data={'id': WS_A_ID}, focused=False,
                                name='A0'),
                SimpleNamespace(ipc_data={'id': WS_B_ID}, focused=True,
                                name='B0'),
            ]

        def get_tree(self) -> FakeTree:
            return FakeTree()

    monkeypatch.setattr(daemon, '_reflow_ncol', fake_reflow)
    monkeypatch.setattr(daemon, '_refetch', lambda _i3, con: con)
    monkeypatch.setattr(daemon, '_refocus_window', lambda _i3, _win: None)

    daemon._ws_states[WS_A_ID] = daemon.WorkspaceState(ws_id=WS_A_ID)
    daemon._ws_states[WS_B_ID] = daemon.WorkspaceState(ws_id=WS_B_ID)
    try:
        daemon._run_existing_layouts(cast(daemon.SwayConn, FakeConn()))
    finally:
        daemon._ws_states.pop(WS_A_ID, None)
        daemon._ws_states.pop(WS_B_ID, None)

    a_calls = [c for c in reflow_calls if c[0] == WS_A_ID]
    b_calls = [c for c in reflow_calls if c[0] == WS_B_ID]
    assert a_calls and not a_calls[0][1], (
        f'WS_A (non-focused) must have is_focused=False, got {a_calls}')
    assert b_calls and b_calls[0][1], (
        f'WS_B (focused) must have is_focused=True, got {b_calls}')


def test_run_existing_layouts_does_not_reset_state_on_sparse_workspace(
        monkeypatch) -> None:
    # Regression: window open/close on any display triggers
    # _run_existing_layouts for ALL workspaces. Workspaces with <=1 tiling
    # window must NOT have their
    # layout state reset. Without this fix, custom n_masters/kind/transforms
    # on displays other than the active one were silently cleared.
    WS_A_ID = 42   # workspace with >=2 windows (active layout)
    WS_B_ID = 77   # workspace with 1 window on "other display"

    class FakeLeaf:
        floating = 'user_off'
        type = 'con'

    class FakeWsA:
        id = WS_A_ID
        layout = 'splith'

        def leaves(self) -> list[FakeLeaf]:
            return [FakeLeaf(), FakeLeaf()]

        def find_focused(self) -> None:
            return None

    class FakeWsB:
        id = WS_B_ID
        layout = 'splith'

        def leaves(self) -> list[FakeLeaf]:
            return [FakeLeaf()]

        def find_focused(self) -> None:
            return None

    ws_a_inst = FakeWsA()
    ws_b_inst = FakeWsB()

    class FakeTree:
        def find_by_id(self, ws_id: int) -> object:
            return ws_a_inst if ws_id == WS_A_ID else ws_b_inst

    class FakeConn:
        def get_workspaces(self) -> list:
            from types import SimpleNamespace
            return [SimpleNamespace(ipc_data={'id': WS_A_ID}, name='A0'),
                    SimpleNamespace(ipc_data={'id': WS_B_ID}, name='B0')]

        def get_tree(self) -> FakeTree:
            return FakeTree()

    # Set custom state on ws_b (the "other display")
    state_b = daemon.WorkspaceState(
        ws_id=WS_B_ID, kind=daemon.LayoutKind.THREE_COL,
        n_masters=2, transforms={daemon.Transform.REFLECTX})
    daemon._ws_states[WS_B_ID] = state_b

    # _reflow_ncol must not block (returns False = nothing moved)
    monkeypatch.setattr(daemon, '_reflow_ncol',
                        lambda _i3, _s, _ws, _if=False: False)
    monkeypatch.setattr(daemon, '_get_focused_workspace', lambda _i3: None)

    try:
        daemon._run_existing_layouts(cast(daemon.SwayConn, FakeConn()))
    finally:
        daemon._ws_states.pop(WS_B_ID, None)

    # ws_b's state must be completely unchanged
    assert state_b.kind == daemon.LayoutKind.THREE_COL
    assert state_b.n_masters == 2
    assert state_b.transforms == {daemon.Transform.REFLECTX}


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
                        lambda _i3, _state, _ws, _if=False: False)
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

    daemon._run_ncol_layout(cast(daemon.SwayConn, FakeConn2()), state, WS_ID)

    assert refocused_ids == [MOVED_WIN_ID], (
        f'Expected focus on moved window {MOVED_WIN_ID}, got {refocused_ids}')


# -----------------------------------------------------------------------------
# --------- on_window move / _swap_moved_before_active -----------------------
# -----------------------------------------------------------------------------
def test_on_window_move_swaps_before_reflow(monkeypatch) -> None:
    '''User-initiated move swaps moved window before active, then reflows.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    monkeypatch.setattr(daemon, '_swap_moved_before_active',
                        lambda _i3, _id: actions.append('swap'))
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='move',
                                      container=SimpleNamespace(id=77)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert actions == ['start', 'swap', 'run', 'flush']


def test_on_window_move_different_id_not_suppressed(monkeypatch) -> None:
    '''Move event for a container NOT in _daemon_move_ids is processed.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    daemon._daemon_move_ids[99] = float('inf')  # different container
    monkeypatch.setattr(daemon, '_swap_moved_before_active',
                        lambda _i3, _id: actions.append('swap'))
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='move',
                                      container=SimpleNamespace(id=77)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    # container 77 not in set → user move processed normally
    assert actions == ['start', 'swap', 'run', 'flush']
    # container 99 still in set (different ID, not consumed)
    assert 99 in daemon._daemon_move_ids


def test_on_window_close_cleans_up_daemon_move_id(monkeypatch) -> None:
    '''Close event removes a stale _daemon_move_ids entry.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    # stale entry simulating closed container
    daemon._daemon_move_ids[55] = float('inf')
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))

    event = cast(Any, SimpleNamespace(change='close',
                                      container=SimpleNamespace(id=55)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert 55 not in daemon._daemon_move_ids  # cleaned up
    assert actions == ['start', 'run', 'flush']  # reflow still happens


def test_swap_moved_before_active_swaps_with_predecessor(
        monkeypatch) -> None:
    '''_swap_moved_before_active swaps moved window with the leaf before it.'''
    MOVED_ID = 30
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
    moved_leaf = FakeLeaf(MOVED_ID)
    after_leaf = FakeLeaf(AFTER_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return moved_leaf if con_id == MOVED_ID else None

    class FakeWsCon:
        id = 1
        name = 'A0'

        def leaves(self) -> list[FakeLeaf]:
            return [prev_leaf, moved_leaf, after_leaf]

    moved_leaf.workspace = (  # type: ignore[method-assign]
        lambda: FakeWsCon())

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_moved_before_active(
        cast(daemon.SwayConn, FakeConn()), MOVED_ID)

    assert f'swap container with con_id {PREV_ID}' in moved_leaf.commands
    assert 'focus' in moved_leaf.commands


def test_swap_moved_before_active_swaps_when_predecessor_is_last(
        monkeypatch) -> None:
    '''_swap_moved_before_active swaps even when moved window is last.

    Unlike _swap_new_before_prev, there is no idx==len-1 early-return.
    '''
    MOVED_ID = 30
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
    moved_leaf = FakeLeaf(MOVED_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return moved_leaf if con_id == MOVED_ID else None

    class FakeWsCon:
        id = 1
        name = 'A0'

        def leaves(self) -> list[FakeLeaf]:
            return [prev_leaf, moved_leaf]

    moved_leaf.workspace = (  # type: ignore[method-assign]
        lambda: FakeWsCon())

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_moved_before_active(
        cast(daemon.SwayConn, FakeConn()), MOVED_ID)

    assert f'swap container with con_id {PREV_ID}' in moved_leaf.commands
    assert 'focus' in moved_leaf.commands


def test_swap_moved_before_active_skips_when_first(monkeypatch) -> None:
    '''_swap_moved_before_active does nothing when moved window is first.'''
    MOVED_ID = 10
    OTHER_ID = 20

    class FakeLeaf:
        def __init__(self, con_id: int) -> None:
            self.id = con_id
            self.floating = 'user_off'
            self.type = 'con'
            self.commands: list[str] = []

        def command(self, payload: str) -> None:
            self.commands.append(payload)

    moved_leaf = FakeLeaf(MOVED_ID)
    other_leaf = FakeLeaf(OTHER_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return moved_leaf if con_id == MOVED_ID else None

    class FakeWsCon:
        id = 1
        name = 'A0'

        def leaves(self) -> list[FakeLeaf]:
            return [moved_leaf, other_leaf]

    moved_leaf.workspace = (  # type: ignore[method-assign]
        lambda: FakeWsCon())

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_moved_before_active(
        cast(daemon.SwayConn, FakeConn()), MOVED_ID)

    assert moved_leaf.commands == []


def test_swap_moved_before_active_skips_temp_workspace() -> None:
    '''_swap_moved_before_active does nothing when destination is a temp ws.

    Evacuation moves in fix_workspace_order land on __swh_tmp_* workspaces.
    Focusing those windows would steal focus from the (possibly empty)
    new_ws and trigger sway's auto-deletion of new_ws.
    '''
    MOVED_ID = 30
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
    moved_leaf = FakeLeaf(MOVED_ID)

    class FakeTree:
        def find_by_id(self, con_id: int) -> Optional[FakeLeaf]:
            return moved_leaf if con_id == MOVED_ID else None

    # Destination workspace is a temp evacuation workspace
    class FakeWsCon:
        id = 1
        name = '__swh_tmp_B5'

        def leaves(self) -> list[FakeLeaf]:
            return [prev_leaf, moved_leaf]

    moved_leaf.workspace = (  # type: ignore[method-assign]
        lambda: FakeWsCon())

    class FakeConn:
        def get_tree(self) -> FakeTree:
            return FakeTree()

    daemon._swap_moved_before_active(
        cast(daemon.SwayConn, FakeConn()), MOVED_ID)

    # No swap and no focus change should have occurred
    assert moved_leaf.commands == []


def test_on_window_expired_daemon_move_not_suppressed(monkeypatch) -> None:
    '''An expired _daemon_move_ids entry must not suppress a user move.'''
    actions: list[str] = []

    class FakeConn:
        def start_buffering(self) -> None:
            actions.append('start')

        def flush(self) -> None:
            actions.append('flush')

    daemon._daemon_move_ids.clear()
    daemon._daemon_move_ids[42] = -1.0  # already expired (negative monotonic)
    monkeypatch.setattr(daemon, '_run_existing_layouts',
                        lambda _i3: actions.append('run'))
    monkeypatch.setattr(daemon, '_swap_moved_before_active',
                        lambda _i3, _id: actions.append('swap'))

    event = cast(Any, SimpleNamespace(change='move',
                                      container=SimpleNamespace(id=42)))
    daemon.on_window(cast(daemon.SwayConn, FakeConn()), event)

    assert 42 not in daemon._daemon_move_ids   # entry consumed regardless
    assert actions == ['start', 'swap', 'run', 'flush']  # treated as user move


def test_run_existing_layouts_continues_after_workspace_error(
        monkeypatch) -> None:
    '''A layout error on one workspace must not abort tiling of the others.'''
    calls: list[int] = []

    class FakeConn:
        def get_workspaces(self) -> list[SimpleNamespace]:
            return [
                SimpleNamespace(ipc_data={'id': 11}, focused=False,
                                name='A0'),
                SimpleNamespace(ipc_data={'id': 22}, focused=False,
                                name='B0'),
                SimpleNamespace(ipc_data={'id': 33}, focused=False,
                                name='C0'),
            ]

        def discard(self) -> None:
            pass

        def start_buffering(self) -> None:
            pass

    def fake_run_layout(_i3: object, ws_id: int,
                        _focused_ws_id: Optional[int] = None) -> None:
        if ws_id == 22:
            raise RuntimeError('simulated workspace error')
        calls.append(ws_id)

    monkeypatch.setattr(daemon, '_run_layout', fake_run_layout)
    monkeypatch.setattr(daemon, '_get_focused_workspace', lambda _i3: None)

    daemon._run_existing_layouts(cast(daemon.SwayConn, FakeConn()))

    assert calls == [11, 33]  # workspace 22 failed, but 33 still executed


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
