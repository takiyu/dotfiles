#!/bin/bash
# Toggle screen recorder
PIDFILE=/tmp/wf-recorder.pid
if [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
    # Stop recording
    kill "$(cat "$PIDFILE")"
    rm -f "$PIDFILE"
    notify-send "Recording stopped" "Screen recording stopped"
else
    # Start recording
    GEOMETRY=$(slurp)
    if [ -n "$GEOMETRY" ]; then
        FILENAME=~/screenrec_$(date +%Y%m%d_%H%M%S).mp4
        wf-recorder -a -f "$FILENAME" --geometry "$GEOMETRY" &
        echo $! > "$PIDFILE"
        notify-send "Recording started" "Screen recording started\n${FILENAME}"
    fi
fi
