#!/usr/bin/env bash

# ------------------------------------------------------------------------------
# ------------------------ OpenCode Session Manager TUI ------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------------------------- Constants ----------------------------------
# ------------------------------------------------------------------------------
readonly state_dir="${XDG_STATE_HOME:-${HOME}/.local/state}"
readonly service_file="${state_dir}/opencode/service.json"
readonly tabs_file="${state_dir}/opencode/latest/tui/tabs.json"
readonly session_limit=5000
readonly session_query="limit=${session_limit}&parentID=null&order=desc"
readonly app_title='OpenCode Sessions'
readonly list_top=5
readonly status_running='稼働中'
readonly status_stopped='停止中'

readonly esc=$'\033'
readonly reset="${esc}[0m"
readonly bold="${esc}[1m"
readonly dim="${esc}[2m"
readonly accent="${esc}[38;5;45m"
readonly green="${esc}[38;5;114m"
readonly gray="${esc}[38;5;245m"
readonly red="${esc}[38;5;203m"
readonly yellow="${esc}[38;5;221m"
readonly header_bg="${esc}[48;5;235m"
readonly footer_bg="${esc}[48;5;235m"
readonly selected_bg="${esc}[48;5;24m"
readonly rule_color="${esc}[38;5;240m"


# ------------------------------------------------------------------------------
# ------------------------------- Prerequisites --------------------------------
# ------------------------------------------------------------------------------
# Required command checks
for command_name in jq curl base64 tput date stty; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    echo "Required command not found: ${command_name}" >&2
    exit 1
  fi
done

if [ -z "${TERM:-}" ]; then
  echo 'TERM is not set; this TUI requires an interactive terminal.' >&2
  exit 1
fi

if [ ! -t 0 ] || [ ! -t 1 ]; then
  echo 'This TUI requires an interactive terminal.' >&2
  exit 1
fi

if [ "$(tput lines 2>/dev/null || echo 0)" -lt 14 ]; then
  echo 'This TUI requires a terminal at least 14 lines tall.' >&2
  exit 1
fi


# ------------------------------------------------------------------------------
# ------------------------------ Session Manager -------------------------------
# ------------------------------------------------------------------------------
declare -A active_ids=()
declare -A date_cache=()
declare -A tabbed_ids=()
running=1
selected_index=0
scroll_offset=0
older_count=0
untabbed_stopped_count=0
reload=1
load_error=''
flash_message=''
confirm_action=''
confirm_message=''
delete_label=''
confirm_ids=()
older_stopped_ids=()
untabbed_stopped_ids=()
server_url=''
auth_header=''
detail_status=''
term_lines=24
term_cols=80


# ------------------------------------------------------------------------------
# --------------------------------- Actions ------------------------------------
# ------------------------------------------------------------------------------
# Load sessions and active ids from the service
load_sessions() {
  printf '\033[2J\033[H'
  printf '%s  ◆ %s%s\n\n  %s読み込み中...%s\n' \
    "$accent" "$app_title" "$reset" "$dim" "$reset"
  load_error=''
  active_ids=()
  date_cache=()
  tabbed_ids=()
  session_ids=()
  session_titles=()
  session_dirs=()
  session_created=()
  session_statuses=()
  session_tabbed=()
  session_count=0
  running_count=0
  stopped_count=0
  tabbed_count=0
  tabs_available=0
  if [ ! -r "$service_file" ]; then
    load_error="サービス情報がありません: ${service_file}"
  fi
  if [ -z "$load_error" ]; then
    server_url=$(jq -r '.url // empty' "$service_file" 2>/dev/null)
    password=$(jq -r '.password // empty' "$service_file" 2>/dev/null)
    if [ -z "$server_url" ] || [ -z "$password" ]; then
      load_error="サービス情報を読み込めません: ${service_file}"
    fi
  fi
  if [ -z "$load_error" ]; then
    credentials=$(printf 'opencode:%s' "$password" | base64 | tr -d '\n')
    auth_header="Authorization: Basic ${credentials}"
    fetch_active_ids
    fetch_status=$?
    if [ 1 -eq "$fetch_status" ]; then
      load_error="OpenCode サービスに接続できません: ${server_url}"
    elif [ 2 -eq "$fetch_status" ]; then
      load_error='アクティブセッションを解析できません'
    fi
  fi
  if [ -z "$load_error" ]; then
    if ! sessions_json=$(curl -fsS --max-time 15 -H "$auth_header" \
      "${server_url}/api/session?${session_query}" 2>/dev/null); then
      load_error="セッション一覧を取得できません: ${server_url}"
    fi
  fi
  if [ -z "$load_error" ]; then
    if [ -r "$tabs_file" ]; then
      if tab_ids_json=$(jq -r '
        [
          ((.global.tabs // [])[] | .sessionID),
          ((.cwd // {}) | .[] | (.tabs // [])[] | .sessionID)
        ] | .[]
      ' "$tabs_file" 2>/dev/null); then
        tabs_available=1
        while IFS= read -r tab_id; do
          if [ -n "$tab_id" ]; then
            tabbed_ids["$tab_id"]=1
          fi
        done <<<"$tab_ids_json"
      fi
    fi
  fi
  if [ -z "$load_error" ]; then
    if ! sessions_rows=$(jq -r '
      .data
      | sort_by(.time.created)
      | reverse[]
      | [.id,
         ((.title // "")
           | explode
           | map(if . < 32 or . == 127 then 32 else . end)
           | implode),
         ((.location.directory // "")
           | explode
           | map(if . < 32 or . == 127 then 32 else . end)
           | implode),
         ((.time.created // 0) | tostring)]
      | join("\u001f")
    ' <<<"$sessions_json" 2>/dev/null); then
      load_error='セッション一覧を解析できません'
    fi
  fi
  if [ -z "$load_error" ]; then
    while IFS=$'\x1f' read -r session_id title directory created; do
      if [ -z "$session_id" ]; then
        continue
      fi
      status=$status_stopped
      if [ -n "${active_ids[$session_id]:-}" ]; then
        status=$status_running
        running_count=$((running_count + 1))
      else
        stopped_count=$((stopped_count + 1))
      fi
      tabbed_flag=0
      if [ -n "${tabbed_ids[$session_id]:-}" ]; then
        tabbed_flag=1
        tabbed_count=$((tabbed_count + 1))
      fi
      session_ids+=("$session_id")
      session_titles+=("$title")
      session_dirs+=("$directory")
      session_created+=("$created")
      session_statuses+=("$status")
      session_tabbed+=("$tabbed_flag")
    done <<<"$sessions_rows"
    session_count=${#session_ids[@]}
    if [ "$session_count" -le "$selected_index" ]; then
      selected_index=$((session_count - 1))
    fi
    if [ "$selected_index" -lt 0 ]; then
      selected_index=0
    fi
  else
    session_count=0
  fi
  reload=0
}

# Render the whole screen
render_screen() {
  # Terminal and scroll layout
  term_lines=$(tput lines 2>/dev/null || echo 24)
  term_cols=$(tput cols 2>/dev/null || echo 80)
  if [ "$term_lines" -lt 14 ]; then
    term_lines=14
  fi
  if [ "$term_cols" -lt 40 ]; then
    term_cols=40
  fi
  list_height=$((term_lines - 8))
  if [ "$list_height" -lt 1 ]; then
    list_height=1
  fi
  if [ 0 -lt "$session_count" ]; then
    if [ "$selected_index" -lt "$scroll_offset" ]; then
      scroll_offset=$selected_index
    fi
    if [ "$selected_index" -ge $((scroll_offset + list_height)) ]; then
      scroll_offset=$((selected_index - list_height + 1))
    fi
    max_offset=$((session_count - list_height))
    if [ "$max_offset" -lt 0 ]; then
      max_offset=0
    fi
    if [ "$scroll_offset" -gt "$max_offset" ]; then
      scroll_offset=$max_offset
    fi
    if [ "$scroll_offset" -lt 0 ]; then
      scroll_offset=0
    fi
  else
    scroll_offset=0
  fi

  # Header rendering
  printf '\033[1;1H%s%s%s ◆ %s\033[K%s' \
    "$header_bg" "$accent" "$bold" "$app_title" "$reset"
  summary="${header_bg}  ${green}● ${running_count} 稼働"
  summary+="${header_bg}  ${gray}● ${stopped_count} 停止"
  if [ 1 -eq "$tabs_available" ]; then
    summary+="${header_bg}  タブ ${tabbed_count}"
  else
    summary+="${header_bg}  タブ ?"
  fi
  summary+="${header_bg}  全 ${session_count}"
  printf '\033[2;1H%s\033[K%s' "$summary" "$reset"
  printf -v rule '%*s' "$term_cols" ''
  rule=${rule// /─}
  printf '\033[3;1H%s%s%s' "$rule_color" "$rule" "$reset"
  printf '\033[%d;1H%s  STATUS  TAB  DATE              TITLE%s\033[K' \
    "$((list_top - 1))" "$dim" "$reset"

  # Session list rendering
  row=$list_top
  if [ -n "$load_error" ]; then
    printf '\033[%d;1H%s%s%s\033[K' "$list_top" "$red" "$load_error" "$reset"
    row=$((row + 1))
  elif [ 0 -eq "$session_count" ]; then
    printf '\033[%d;1H%s  セッションがありません%s\033[K' \
      "$list_top" "$dim" "$reset"
    row=$((row + 1))
  else
    for ((i = scroll_offset; i < scroll_offset + list_height; i++)); do
      if [ "$i" -ge "$session_count" ]; then
        break
      fi
      if [ "$i" -eq "$selected_index" ]; then
        row_bg=$selected_bg
        marker='>'
        date_color=''
        title_style=$bold
      else
        row_bg=''
        marker=' '
        date_color=$dim
        title_style=''
      fi
      if [ "$status_running" = "${session_statuses[$i]}" ]; then
        status_color=$green
      else
        status_color=$gray
      fi
      if [ 1 -eq "${session_tabbed[$i]}" ]; then
        tab_label='TAB'
        tab_color=$accent
      else
        tab_label=' - '
        tab_color=$dim
      fi
      if [ -n "${date_cache[$i]:-}" ]; then
        date_text=${date_cache[$i]}
      else
        created=${session_created[$i]}
        date_text=$(date -d "@$((created / 1000))" '+%Y-%m-%d %H:%M')
        date_cache[$i]="$date_text"
      fi
      row_line="${row_bg}${accent}${marker}${reset}${row_bg}"
      row_line+=" ${status_color}${session_statuses[$i]}"
      row_line+="${reset}${row_bg}  ${tab_color}${tab_label}"
      row_line+="${reset}${row_bg}  ${date_color}${date_text}"
      row_line+="${reset}${row_bg}  ${title_style}${session_titles[$i]}"
      printf '\033[%d;1H%s\033[K%s' "$row" "$row_line" "$reset"
      row=$((row + 1))
    done
  fi
  while [ "$row" -lt $((list_top + list_height)) ]; do
    printf '\033[%d;1H\033[K' "$row"
    row=$((row + 1))
  done

  collect_delete_candidates

  # Detail and footer rendering
  printf '\033[%d;1H%s%s%s' \
    "$((term_lines - 3))" "$rule_color" "$rule" "$reset"
  if [ 0 -lt "$session_count" ]; then
    detail_title=${session_titles[$selected_index]}
    detail_dir=${session_dirs[$selected_index]}
    detail_id=${session_ids[$selected_index]}
    detail_status=${session_statuses[$selected_index]}
    detail_line="${footer_bg}${accent} ▶ ${reset}${footer_bg}${bold}"
    detail_line+="${detail_title}${reset}${footer_bg}"
    printf '\033[%d;1H%s\033[K%s' \
      "$((term_lines - 2))" "$detail_line" "$reset"
    detail_line="${footer_bg}  ${dim}${detail_dir}"
    detail_line+="${reset}${footer_bg}  ${dim}${detail_id}"
    detail_line+="${reset}${footer_bg}"
    printf '\033[%d;1H%s\033[K%s' \
      "$((term_lines - 1))" "$detail_line" "$reset"
  else
    printf '\033[%d;1H\033[K' "$((term_lines - 2))"
    printf '\033[%d;1H\033[K' "$((term_lines - 1))"
    detail_status=''
  fi
  if [ -n "$flash_message" ]; then
    footer_line="${footer_bg}${yellow} ${flash_message}"
    footer_line+="${reset}${footer_bg}"
  elif [ -n "$confirm_action" ]; then
    footer_line="${footer_bg}${yellow} ${confirm_message} [y/N]"
    footer_line+="${reset}${footer_bg}"
  elif [ -n "$load_error" ]; then
    footer_line="${footer_bg}${red} ${load_error}"
    footer_line+="${reset}${footer_bg}  ${dim}r: 再試行  q/Esc: 終了"
    footer_line+="${reset}${footer_bg}"
  elif [ 0 -eq "$session_count" ]; then
    footer_line="${footer_bg} ${accent}r${reset}${footer_bg}: 更新   "
    footer_line+="${accent}q/Esc${reset}${footer_bg}: 終了"
  else
    if [ "$status_running" = "$detail_status" ]; then
      action_label='停止'
    else
      action_label='削除'
    fi
    footer_line="${footer_bg} ${accent}j/k${reset}${footer_bg}: 移動  "
    footer_line+="${accent}Enter${reset}${footer_bg}: ${action_label}  "
    footer_line+="${accent}d${reset}${footer_bg}: 古い停止(${older_count})  "
    if [ 1 -eq "$tabs_available" ]; then
      footer_line+="${accent}t${reset}${footer_bg}: タブ非表示"
      footer_line+="(${untabbed_stopped_count})  "
    fi
    footer_line+="${accent}r${reset}${footer_bg}: 更新  "
    footer_line+="${accent}q/Esc${reset}${footer_bg}: 終了"
  fi
  printf '\033[%d;1H%s\033[K%s' "$term_lines" "$footer_line" "$reset"
}

# Handle one key press
handle_input() {
  # Key input
  IFS= read -rsn1 key || key='q'
  flash_message=''
  if [ -n "$confirm_action" ]; then
    if [ 'y' != "$key" ] && [ 'Y' != "$key" ]; then
      flash_message='キャンセルしました'
      reload=0
      confirm_action=''
      return
    fi
    case "$confirm_action" in
      'stop_selected')
        selected_id=${session_ids[$selected_index]}
        if interrupt_json=$(curl -fsS --max-time 10 -H "$auth_header" \
          -X POST \
          "${server_url}/api/session/${selected_id}/interrupt" \
          2>/dev/null); then
          if [ 'true' = "$(jq -r '.interrupted' <<<"$interrupt_json")" ]; then
            flash_message='セッションを停止しました'
          else
            flash_message='セッションは既に停止しています'
          fi
        else
          flash_message='停止に失敗しました'
        fi
        ;;
      'delete_selected' | 'delete_older' | 'delete_untabbed')
        delete_confirmed_sessions
        ;;
    esac
    reload=1
    confirm_action=''
    return
  fi

  # Escape sequence parsing
  if [ "$key" = $'\033' ]; then
    rest=''
    rest_extra=''
    IFS= read -rsn2 -t 0.05 rest || true
    case "$rest" in
      '['[0-9])
        IFS= read -rsn1 -t 0.05 rest_extra || true
        rest+="$rest_extra"
        ;;
    esac
    case "$rest" in
      '') key='cancel' ;;
      '[A' | 'OA') key='up' ;;
      '[B' | 'OB') key='down' ;;
      '[5~' | '[1~') key='pageup' ;;
      '[6~' | '[4~') key='pagedown' ;;
      '[H' | 'OH') key='home' ;;
      '[F' | 'OF') key='end' ;;
      *) key='ignore' ;;
    esac
  fi

  # Error state keys
  if [ -n "$load_error" ]; then
    case "$key" in
      'r' | 'R') reload=1 ;;
      'q' | 'Q' | 'cancel') running=0; return ;;
    esac
    return
  fi

  # Navigation and actions
  case "$key" in
    'q' | 'Q' | 'cancel') running=0; return ;;
    'r' | 'R')
      reload=1
      flash_message='再読み込みしました'
      ;;
    'up' | 'k')
      if [ 0 -lt "$selected_index" ]; then
        selected_index=$((selected_index - 1))
      fi
      ;;
    'down' | 'j')
      if [ "$selected_index" -lt $((session_count - 1)) ]; then
        selected_index=$((selected_index + 1))
      fi
      ;;
    'pageup')
      selected_index=$((selected_index - list_height))
      if [ "$selected_index" -lt 0 ]; then
        selected_index=0
      fi
      ;;
    'pagedown')
      if [ 0 -lt "$session_count" ]; then
        selected_index=$((selected_index + list_height))
        if [ "$selected_index" -ge "$session_count" ]; then
          selected_index=$((session_count - 1))
        fi
      fi
      ;;
    'home') selected_index=0 ;;
    'end')
      if [ 0 -lt "$session_count" ]; then
        selected_index=$((session_count - 1))
      fi
      ;;
    'd' | 'D')
      if [ 0 -eq "$session_count" ] || [ 0 -eq "$older_count" ]; then
        flash_message='削除対象がありません'
      else
        confirm_ids=("${older_stopped_ids[@]}")
        confirm_action='delete_older'
        delete_label='選択より古い停止中セッション'
      fi
      ;;
    't' | 'T')
      if [ 0 -eq "$tabs_available" ]; then
        flash_message='タブ情報を読み込めません'
      elif [ 0 -eq "$untabbed_stopped_count" ]; then
        flash_message='削除対象がありません'
      else
        confirm_ids=("${untabbed_stopped_ids[@]}")
        confirm_action='delete_untabbed'
        delete_label='タブ非表示の停止中セッション'
      fi
      ;;
    '' | $'\r' | $'\n')
      if [ 0 -lt "$session_count" ]; then
        if [ "$status_running" = "${session_statuses[$selected_index]}" ]; then
          confirm_action='stop_selected'
          confirm_message='このセッションを停止しますか？'
        else
          confirm_ids=("${session_ids[$selected_index]}")
          confirm_action='delete_selected'
          confirm_message='このセッションを削除しますか？'
        fi
      fi
      ;;
  esac
  if [ 'delete_older' = "$confirm_action" ] ||
    [ 'delete_untabbed' = "$confirm_action" ]; then
    confirm_message="${delete_label} ${#confirm_ids[@]}"
    confirm_message+=' 件を全プロジェクトから削除しますか？'
  fi
}

# Collect stopped sessions older than the selection and untabbed stopped ones
collect_delete_candidates() {
  local selected_created
  older_stopped_ids=()
  untabbed_stopped_ids=()
  if [ 0 -lt "$session_count" ]; then
    selected_created=${session_created[$selected_index]}
    for ((older_index = selected_index + 1;
      older_index < session_count; older_index++)); do
      if [ "${session_created[$older_index]}" -lt "$selected_created" ] &&
        [ "$status_stopped" = "${session_statuses[$older_index]}" ]; then
        older_stopped_ids+=("${session_ids[$older_index]}")
      fi
    done
  fi
  if [ 1 -eq "$tabs_available" ]; then
    for ((tab_index = 0; tab_index < session_count; tab_index++)); do
      if [ "$status_stopped" = "${session_statuses[$tab_index]}" ] &&
        [ 0 -eq "${session_tabbed[$tab_index]}" ]; then
        untabbed_stopped_ids+=("${session_ids[$tab_index]}")
      fi
    done
  fi
  older_count=${#older_stopped_ids[@]}
  untabbed_stopped_count=${#untabbed_stopped_ids[@]}
}

# Delete the session ids in confirm_ids and report the outcome
delete_confirmed_sessions() {
  local delete_id deleted_count=0 failed_count=0 skipped_count=0
  local total_count=${#confirm_ids[@]}
  if ! fetch_active_ids; then
    flash_message='稼働状況を確認できません'
    return
  fi
  for delete_id in "${confirm_ids[@]}"; do
    if [ -n "${active_ids[$delete_id]:-}" ]; then
      skipped_count=$((skipped_count + 1))
      continue
    fi
    if curl -fsS --max-time 10 -o /dev/null -H "$auth_header" \
      -X DELETE "${server_url}/api/session/${delete_id}" 2>/dev/null; then
      deleted_count=$((deleted_count + 1))
    else
      failed_count=$((failed_count + 1))
    fi
    printf '\033[%d;1H%s 削除中... %d/%d %s\033[K' \
      "$term_lines" "$footer_bg" \
      "$((deleted_count + failed_count))" "$total_count" "$reset"
  done
  if [ 0 -lt "$skipped_count" ]; then
    flash_message="${deleted_count} 件削除 / ${skipped_count} 件稼働中"
  elif [ 0 -lt "$failed_count" ]; then
    flash_message="${deleted_count} 件削除 / ${failed_count} 件失敗"
  elif [ 1 -eq "$total_count" ]; then
    flash_message='セッションを削除しました'
  else
    flash_message="${deleted_count} 件削除しました"
  fi
}

# Fetch active session ids into active_ids; returns non-zero on failure
fetch_active_ids() {
  if ! active_json=$(curl -fsS --max-time 5 -H "$auth_header" \
    "${server_url}/api/session/active" 2>/dev/null); then
    return 1
  fi
  if ! active_ids_json=$(jq -r '.data | keys[]' \
    <<<"$active_json" 2>/dev/null); then
    return 2
  fi
  active_ids=()
  while IFS= read -r active_id; do
    if [ -n "$active_id" ]; then
      active_ids["$active_id"]=1
    fi
  done <<<"$active_ids_json"
}


# Screen setup
trap 'stty sane 2>/dev/null; tput cnorm 2>/dev/null; \
tput smam 2>/dev/null; tput rmcup 2>/dev/null' EXIT
trap 'exit 130' INT TERM
tput smcup
tput civis
tput rmam
stty -echo -icanon -icrnl min 1 time 0

while [ 1 -eq "$running" ]; do
  if [ 1 -eq "$reload" ]; then
    load_sessions
  fi
  render_screen
  handle_input
done


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
