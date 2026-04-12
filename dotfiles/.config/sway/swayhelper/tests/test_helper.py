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
