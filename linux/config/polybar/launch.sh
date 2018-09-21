#!/usr/bin/env sh

DP1_STATUS=$(</sys/class/drm/card0/card0-DP-1/status )

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

if [ "connected" == "$DP1_STATUS" ]; then
    MONITOR=DP1 polybar main-ext &
    # MONITOR=eDP1 polybar secondary &
else
    MONITOR=eDP1 polybar main-int &
fi
