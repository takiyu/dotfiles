#!/bin/sh
# Kill existing daemon instances, then exec the new one.
# Called by Makefile daemon target; exec replaces this shell with python.
DIR=$(dirname "$(readlink -f "$0")")
PYTHON="$DIR/.venv/bin/python"

# Kill previous swayhelper daemon instances by command pattern.
# Escape the dot so pgrep treats it as a literal character and does not
# match "swayhelper daemon" in make command lines.
for pid in $(pgrep -f "swayhelper\.daemon"); do
    if [ "$pid" -ne $$ ]; then
        kill "$pid" 2>/dev/null || true
    fi
done

sleep 0.2

rm -f /tmp/swayhelper-daemon.pid
exec "$PYTHON" -m swayhelper.daemon
