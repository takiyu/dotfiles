#!/bin/sh
# Wrapper script launched by sway exec_always to start the tiling daemon.
# Using a script avoids nested-quoting issues in sway config.
LOG=/tmp/swayhelper-daemon.log
DIR=$(dirname "$(readlink -f "$0")")
PYTHON="$DIR/.venv/bin/python"

printf '%s start_daemon.sh: dir=%s\n' "$(date '+%Y-%m-%d %H:%M:%S')" "$DIR" >> "$LOG" 2>&1

# Kill existing daemon by PID if running (avoids pkill PATH dependency)
if [ -f /tmp/swayhelper-daemon.pid ]; then
    OLD_PID=$(cat /tmp/swayhelper-daemon.pid)
    kill "$OLD_PID" 2>/dev/null || true
    rm -f /tmp/swayhelper-daemon.pid
fi

# Also try pgrep as fallback
PIDS=$(pgrep -f 'swayhelper.daemon' 2>/dev/null) && {
    for pid in $PIDS; do
        kill "$pid" 2>/dev/null || true
    done
}

sleep 0.3

# Record PID and exec daemon (exec replaces this shell process)
printf '%s start_daemon.sh: launching daemon (python=%s)\n' \
    "$(date '+%Y-%m-%d %H:%M:%S')" "$PYTHON" >> "$LOG" 2>&1
echo $$ > /tmp/swayhelper-daemon.pid
exec "$PYTHON" -m swayhelper.daemon >> "$LOG" 2>&1
