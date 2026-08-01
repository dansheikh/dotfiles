#!/usr/bin/env bash
# idle-toggle

PROCESS_PATTERN="systemd-inhibit --what=idle --who=caffeine"

notify() {
    if command -v notify-send >/dev/null 2>&1; then
        notify-send -u low -i "$1" "Caffeine" "$2"
    fi
}

if pgrep -f "$PROCESS_PATTERN" >/dev/null 2>&1; then
    pkill -f "$PROCESS_PATTERN"
    notify "display" "Idle inhibition DISABLED."
else
    nohup $PROCESS_PATTERN --why="User toggle" sleep infinity >/dev/null 2>&1 &
    disown
    notify "display" "Idle inhibition ENABLED."
fi

# Refresh Noctalia widgets immediately
noctalia-cli reload >/dev/null 2>&1 || true
