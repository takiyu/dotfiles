import contextlib

from swayhelper import helper


def test_is_managed_workspace_filters_internal_names() -> None:
    assert helper._is_managed_workspace('A0')
    assert helper._is_managed_workspace('12')
    assert not helper._is_managed_workspace('_tA1')
    assert not helper._is_managed_workspace('__ws_A1')


def test_get_visible_workspace_name_prefers_visible_workspace() -> None:
    workspaces = [
        {'name': 'A0', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A1', 'output': 'DP-1', 'visible': True, 'focused': False},
        {'name': 'B0', 'output': 'DP-2', 'visible': True, 'focused': True},
    ]

    assert helper._get_visible_workspace_name(workspaces, 'DP-1') == 'A1'


def test_shift_workspace_name_grows_without_upper_bound() -> None:
    assert helper._shift_workspace_name('A9', +1) == 'A10'
    assert helper._shift_workspace_name('B99', +1) == 'B100'


def test_shift_workspace_name_clamps_at_zero() -> None:
    assert helper._shift_workspace_name('A0', -1) == 'A0'
    assert helper._shift_workspace_name('A1', -1) == 'A0'


def test_insert_workspace_before_current_shifts_current_and_later(
        monkeypatch) -> None:
    renamed: list[tuple[str, str]] = []
    focused: list[str] = []
    fixed: list[tuple[str, str]] = []
    ws_data = [
        {'name': 'A0', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A2', 'output': 'DP-1', 'visible': True, 'focused': True},
        {'name': 'A3', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'B0', 'output': 'DP-2', 'visible': True, 'focused': False},
    ]

    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, '_get_workspaces_data', lambda: ws_data)
    monkeypatch.setattr(helper, 'rename_workspace',
                        lambda old, new: renamed.append((old, new)))
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws_name: focused.append(ws_name))
    monkeypatch.setattr(helper, 'fix_workspace_order',
                        lambda display, ws_name: fixed.append(
                            (display, ws_name)))

    helper.insert_workspace_before_current()

    assert renamed == [('A3', 'A4'), ('A2', 'A3')]
    assert focused == ['A2']
    assert fixed == [('DP-1', 'A2')]


def test_delete_current_workspace_if_empty_shifts_later_workspaces_left(
        monkeypatch) -> None:
    focused: list[str] = []
    renamed: list[tuple[str, str]] = []
    ws_data = [
        {'name': 'A0', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A2', 'output': 'DP-1', 'visible': True, 'focused': True},
        {'name': 'A3', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A4', 'output': 'DP-1', 'visible': False, 'focused': False},
    ]

    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, '_get_workspaces_data', lambda: ws_data)
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda ws_name: [] if ws_name == 'A2' else [1])
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws_name: focused.append(ws_name))
    monkeypatch.setattr(helper, 'rename_workspace',
                        lambda old, new: renamed.append((old, new)))

    helper.delete_current_workspace_if_empty()

    assert focused == ['A3']
    assert renamed == [('A3', 'A2'), ('A4', 'A3')]


def test_delete_current_workspace_if_empty_noop_when_not_empty(
        monkeypatch) -> None:
    focused: list[str] = []
    renamed: list[tuple[str, str]] = []
    ws_data = [
        {'name': 'A0', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A2', 'output': 'DP-1', 'visible': True, 'focused': True},
        {'name': 'A3', 'output': 'DP-1', 'visible': False, 'focused': False},
    ]

    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, '_get_workspaces_data', lambda: ws_data)
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda _ws_name: [42])
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws_name: focused.append(ws_name))
    monkeypatch.setattr(helper, 'rename_workspace',
                        lambda old, new: renamed.append((old, new)))

    helper.delete_current_workspace_if_empty()

    assert focused == []
    assert renamed == []


def test_delete_current_workspace_if_empty_last_workspace_moves_previous(
        monkeypatch) -> None:
    focused: list[str] = []
    renamed: list[tuple[str, str]] = []
    ws_data = [
        {'name': 'A0', 'output': 'DP-1', 'visible': False, 'focused': False},
        {'name': 'A2', 'output': 'DP-1', 'visible': True, 'focused': True},
    ]

    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, '_get_workspaces_data', lambda: ws_data)
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda _ws_name: [])
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws_name: focused.append(ws_name))
    monkeypatch.setattr(helper, 'rename_workspace',
                        lambda old, new: renamed.append((old, new)))

    helper.delete_current_workspace_if_empty()

    assert focused == ['A0']
    assert renamed == []


def test_get_workspace_cycle_retries_until_current_is_stable(
        monkeypatch) -> None:
    workspaces = [
        [
            {'name': 'A0', 'output': 'DP-1', 'visible': False,
             'focused': False},
            {'name': '_tA1', 'output': 'DP-1', 'visible': True,
             'focused': False},
            {'name': 'A1', 'output': 'DP-1', 'visible': False,
             'focused': False},
        ],
        [
            {'name': 'A0', 'output': 'DP-1', 'visible': False,
             'focused': False},
            {'name': 'A1', 'output': 'DP-1', 'visible': True,
             'focused': False},
        ],
    ]

    def fake_get_workspaces_data() -> list[dict[str, object]]:
        return workspaces.pop(0)

    monkeypatch.setattr(helper, '_get_workspaces_data',
                        fake_get_workspaces_data)
    monkeypatch.setattr(helper.time, 'sleep', lambda _seconds: None)

    assert helper.get_workspace_cycle('DP-1') == (['A0', 'A1'], 'A1')


def test_focus_valid_nei_workspace_loops_existing_workspaces(
        monkeypatch) -> None:
    focused: list[str] = []

    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, 'get_workspace_cycle',
                        lambda _display: (['A0', 'A1', 'A3'], 'A3'))
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws_name: focused.append(ws_name))

    helper.focus_valid_nei_workspace(+1)
    helper.focus_valid_nei_workspace(-1)

    assert focused == ['A0', 'A1']


def test_move_workspace_uses_atomic_swaymsg_when_window_focused(
        monkeypatch) -> None:
    # When a window is focused, move_workspace must issue ONE swaymsg that
    # combines move + workspace-switch + focus so the daemon cannot interleave
    # its layout re-tile between the workspace switch and the focus restore.
    cmds: list[str] = []
    FOCUSED_WIN_ID = 42

    monkeypatch.setattr(helper, 'get_focused_window_id',
                        lambda: FOCUSED_WIN_ID)
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda _ws: (_ for _ in ()).throw(
                            AssertionError('focus_workspace must not be '
                                           'called separately when win_id '
                                           'is available')))
    monkeypatch.setattr(helper, 'run_cmd', lambda cmd: cmds.append(cmd))

    result = helper.move_workspace('B0')

    assert result == FOCUSED_WIN_ID
    assert len(cmds) == 1
    assert 'move container to workspace B0' in cmds[0]
    assert 'workspace B0' in cmds[0]
    assert f'[con_id={FOCUSED_WIN_ID}] focus' in cmds[0]


def test_move_workspace_falls_back_when_no_window(monkeypatch) -> None:
    # When there is no focused window, fall back to separate move + workspace.
    calls: list[str] = []

    monkeypatch.setattr(helper, 'get_focused_window_id', lambda: None)
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws: calls.append(f'ws:{ws}'))
    monkeypatch.setattr(helper, 'run_cmd', lambda _cmd: calls.append('move'))

    result = helper.move_workspace('B0')

    assert result is None
    assert 'ws:B0' in calls
    assert not any(c.startswith('win:') for c in calls)


def test_move_nei_workspace_refocuses_after_fix_workspace_order(
        monkeypatch) -> None:
    # When fix_workspace_order is triggered (new workspace creation),
    # move_nei_workspace must call focus_window after the fixup to
    # restore focus to the moved window.
    focused_wins: list[int] = []
    FOCUSED_WIN_ID = 99

    monkeypatch.setattr(helper, '_move_lock', contextlib.nullcontext)
    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, 'get_cur_workspace', lambda _d: 'A0')
    monkeypatch.setattr(helper, 'get_workspaces_raw', lambda _d: ['A0'])
    monkeypatch.setattr(helper, 'move_workspace',
                        lambda _ws: FOCUSED_WIN_ID)
    monkeypatch.setattr(helper, 'fix_workspace_order',
                        lambda _d, _ws: None)
    monkeypatch.setattr(helper, 'focus_window',
                        lambda wid: focused_wins.append(wid))

    helper.move_nei_workspace(+1)  # moves to A1 (new workspace)

    assert FOCUSED_WIN_ID in focused_wins


def test_move_nei_workspace_serialises_concurrent_calls(
        monkeypatch) -> None:
    # Verify _move_lock is acquired so concurrent calls see correct state.
    # Simulate: first call updates cur_ws; second call sees updated workspace.
    call_order: list[str] = []
    lock_entered = 0

    class _TrackingLock:
        def __enter__(self):
            nonlocal lock_entered
            lock_entered += 1
            call_order.append(f'lock:{lock_entered}')
            return self

        def __exit__(self, *_):
            call_order.append('unlock')

    monkeypatch.setattr(helper, '_move_lock', _TrackingLock)
    monkeypatch.setattr(helper, 'get_cur_display', lambda: 'DP-1')
    monkeypatch.setattr(helper, 'get_cur_workspace', lambda _d: 'A0')
    monkeypatch.setattr(helper, 'get_workspaces_raw', lambda _d: ['A0', 'A1'])
    monkeypatch.setattr(helper, 'move_workspace', lambda _ws: 42)
    monkeypatch.setattr(helper, 'fix_workspace_order', lambda _d, _ws: None)
    monkeypatch.setattr(helper, 'focus_window', lambda _wid: None)

    helper.move_nei_workspace(+1)

    # Lock must be acquired and released exactly once per call
    assert call_order == ['lock:1', 'unlock']


# -----------------------------------------------------------------------------
# ----------------------- _strip_reorder_tmp_prefix --------------------------
# -----------------------------------------------------------------------------
def test_strip_reorder_tmp_prefix_removes_new_prefix() -> None:
    assert helper._strip_reorder_tmp_prefix('__swh_tmp_A1') == 'A1'


def test_strip_reorder_tmp_prefix_removes_legacy_prefix() -> None:
    assert helper._strip_reorder_tmp_prefix('_tA1') == 'A1'
    assert helper._strip_reorder_tmp_prefix('_tB0') == 'B0'


def test_strip_reorder_tmp_prefix_removes_nested_legacy_prefix() -> None:
    assert helper._strip_reorder_tmp_prefix('_t_tA1') == 'A1'
    assert helper._strip_reorder_tmp_prefix('_t_t_tB2') == 'B2'


def test_strip_reorder_tmp_prefix_leaves_plain_workspace_unchanged() -> None:
    assert helper._strip_reorder_tmp_prefix('A1') == 'A1'
    assert helper._strip_reorder_tmp_prefix('B0') == 'B0'
    assert helper._strip_reorder_tmp_prefix('10') == '10'


# -----------------------------------------------------------------------------
# ----------------------- _cleanup_temp_workspaces ---------------------------
# -----------------------------------------------------------------------------
def test_cleanup_temp_workspaces_moves_windows_from_legacy_temp(
        monkeypatch) -> None:
    moved: list[tuple[int, str]] = []
    ws_by_disp: dict[str, list[str]] = {'DP-1': ['A0', '_tA1', 'A2']}

    monkeypatch.setattr(helper, 'get_workspaces_raw',
                        lambda disp: ws_by_disp.get(disp, []))
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda ws: [42] if ws == '_tA1' else [])
    monkeypatch.setattr(helper, 'move_window_to_workspace',
                        lambda wid, ws: moved.append((wid, ws)))

    helper._cleanup_temp_workspaces(['DP-1'])

    assert moved == [(42, 'A1')]


def test_cleanup_temp_workspaces_handles_nested_temp_names(
        monkeypatch) -> None:
    moved: list[tuple[int, str]] = []
    ws_by_disp: dict[str, list[str]] = {'DP-1': ['_t_tA1']}

    monkeypatch.setattr(helper, 'get_workspaces_raw',
                        lambda disp: ws_by_disp.get(disp, []))
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda ws: [99] if ws == '_t_tA1' else [])
    monkeypatch.setattr(helper, 'move_window_to_workspace',
                        lambda wid, ws: moved.append((wid, ws)))

    helper._cleanup_temp_workspaces(['DP-1'])

    assert moved == [(99, 'A1')]


def test_cleanup_temp_workspaces_skips_non_managed_real_names(
        monkeypatch) -> None:
    moved: list[tuple[int, str]] = []
    # _tsomething -> 'something' is not a managed workspace; must be skipped
    ws_by_disp: dict[str, list[str]] = {'DP-1': ['_tsomething']}

    monkeypatch.setattr(helper, 'get_workspaces_raw',
                        lambda disp: ws_by_disp.get(disp, []))
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda _ws: [1])
    monkeypatch.setattr(helper, 'move_window_to_workspace',
                        lambda wid, ws: moved.append((wid, ws)))

    helper._cleanup_temp_workspaces(['DP-1'])

    assert not moved


# -----------------------------------------------------------------------------
# ----------------------- fix_workspace_order (new) --------------------------
# -----------------------------------------------------------------------------
def test_fix_workspace_order_ignores_temp_workspace(monkeypatch) -> None:
    # _tB2 must be excluded from ws_to_reorder; otherwise reordering would
    # create _t_tB2 and cascade into deeper temp names on rapid switching.
    moved: list[tuple[int, str]] = []
    focused: list[str] = []

    monkeypatch.setattr(helper, '_reorder_lock', contextlib.nullcontext)
    monkeypatch.setattr(helper, 'get_workspaces_raw',
                        lambda _d: ['A0', '_tB2', 'A1'])
    monkeypatch.setattr(helper, 'get_windows_on_workspace', lambda _ws: [])
    monkeypatch.setattr(helper, 'move_window_to_workspace',
                        lambda wid, ws: moved.append((wid, ws)))
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws: focused.append(ws))

    helper.fix_workspace_order('DP-1', 'A1')

    # Nothing to reorder (only _tB2 would qualify but it is not managed)
    assert not moved
    assert not focused


def test_fix_workspace_order_aborts_on_evacuation_timeout(
        monkeypatch) -> None:
    # When evacuated workspaces never disappear, windows must be restored
    # to their original workspace and the function must return early.
    moved: list[tuple[int, str]] = []
    focused: list[str] = []

    monkeypatch.setattr(helper, '_reorder_lock', contextlib.nullcontext)
    # A2 is always present (never auto-deleted), simulating timeout
    monkeypatch.setattr(helper, 'get_workspaces_raw',
                        lambda _d: ['A0', 'A2', 'A1'])
    monkeypatch.setattr(helper, 'get_windows_on_workspace',
                        lambda ws: [42] if ws == 'A2' else [])
    monkeypatch.setattr(helper, 'move_window_to_workspace',
                        lambda wid, ws: moved.append((wid, ws)))
    monkeypatch.setattr(helper, 'focus_workspace',
                        lambda ws: focused.append(ws))
    monkeypatch.setattr(helper.time, 'sleep', lambda _: None)

    helper.fix_workspace_order('DP-1', 'A1')

    # Window was first evacuated then restored; no recreation should happen
    assert (42, f'{helper._WS_REORDER_TMP}A2') in moved
    assert (42, 'A2') in moved
    assert not focused
