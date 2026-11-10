#!/bin/bash
# ------------------------------------------------------------------------------
# ------------------ IME Switching Daemon for sway Terminals -------------------
# ------------------------------------------------------------------------------

# ------------------------------- Configuration --------------------------------
TERMINALS="xfce4-terminal foot"
# opencode window titles: "OC | <session>" or the plain brand name
OPENCODE_TITLE_RE='OC \| |OpenCode'
STATE_ON="2"
STATE_OFF="1"
RETRY_MAX=8
RETRY_INTERVAL=0.08


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------ Implementation --------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------- Event Watching -------------------------------
# Single instance guard across sway reloads
exec 9>"${XDG_RUNTIME_DIR:-/tmp}/ime_focus_watch.lock"
flock -w 5 9 || exit 1

while true; do
    # Reading focus and title events from sway
    while IFS=$'\t' read -r app_id title; do
        # Skipping windows that are not terminals
        case " $TERMINALS " in
            *" $app_id "*) ;;
            *) continue ;;
        esac
        # Choosing the target state from the opencode window title
        if [[ $title =~ $OPENCODE_TITLE_RE ]]; then
            flag="-o"
            want="$STATE_ON"
        else
            flag="-c"
            want="$STATE_OFF"
        fi
        # Applying and verifying the input method state
        for ((i = 0; i < RETRY_MAX; i++)); do
            current=$(fcitx5-remote 2>/dev/null)
            if [[ $current == "$want" ]]; then
                break
            fi
            fcitx5-remote "$flag" >/dev/null 2>&1
            sleep "$RETRY_INTERVAL"
        done
    done < <(
        # Releasing the lock in the event source processes
        exec 9>&-
        # Evaluating the currently focused window once at startup
        swaymsg -t get_tree | jq -r '
            [.. | objects | select(.focused? == true)][0] |
            [(.app_id // ""), (.name // "")] | @tsv'
        # Streaming focus and title events from sway
        exec swaymsg -t subscribe -m '["window"]' | jq --unbuffered -r '
            select(.change == "focus" or .change == "title") |
            [(.container.app_id // ""), (.container.name // "")] | @tsv'
    )
    # Reconnecting after the event stream ends
    sleep 1
done


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
