#!/bin/sh
# Kill existing daemon instances, then exec the new one.
# Called by Makefile daemon target; exec replaces this shell with python.
DIR=$(dirname "$(readlink -f "$0")")
PYTHON="$DIR/.venv/bin/python"

# Kill previous instance via PID file
if [ -f /tmp/swayhelper-daemon.pid ]; then
    OLD_PID=$(cat /tmp/swayhelper-daemon.pid)
    kill "$OLD_PID" 2>/dev/null || true
    rm -f /tmp/swayhelper-daemon.pid
fi

sleep 0.2

# Write PID then exec daemon (exec replaces this shell with python)
echo $$ > /tmp/swayhelper-daemon.pid
exec "$PYTHON" -m swayhelper.daemon
