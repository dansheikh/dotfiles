#!/usr/bin/env bash
#
# idle-toggle - Toggles systemd-inhibit to keep the system awake or allow normal idle behavior.

# Unique pattern used for both process identification and invocation
PROCESS_PATTERN="systemd-inhibit --what=idle --who=caffeine"

# Helper for desktop notifications (silently skipped if notify-send isn't installed)
notify() {
    if command -v notify-send >/dev/null 2>&1; then
        notify-send -u low -i "$1" "Caffeine" "$2"
    fi
}

# Check if an inhibitor instance is already running
if pgrep -f "$PROCESS_PATTERN" >/dev/null 2>&1; then
    # Currently Active -> Turn OFF
    pkill -f "$PROCESS_PATTERN"
    notify "display" "Idle inhibition DISABLED. Normal power management restored."
else
    # Currently Inactive -> Turn ON
    # Spawns in background (&) and disowns it so closing the launching shell won't kill it
    nohup $PROCESS_PATTERN --why="User toggle" sleep infinity >/dev/null 2>&1 &
    disown
    notify "display" "Idle inhibition ENABLED. System will stay awake."
fi
