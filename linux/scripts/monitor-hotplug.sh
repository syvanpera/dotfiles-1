#!/bin/sh

# Use only for i3wm, budgie etc will handle this themselves
if [ ! "i3" == $DESKTOP_SESSION ]; then
  exit
fi

# Get out of town if something errors
set -e

DP1_STATUS=$(</sys/class/drm/card0/card0-DP-1/status )

if [ "connected" == "$DP1_STATUS" ]; then
  /usr/bin/xrandr --output DP1 --right-of eDP1 --auto --primary --scale 1x1 --dpi 110
  /usr/bin/notify-send --urgency=low -t 5000 "Graphics Update" "External monitor plugged in"
else
  /usr/bin/xrandr --output DP1 --off --output eDP1 --auto --primary --scale 1x1 --dpi 260
  /usr/bin/notify-send --urgency=low -t 5000 "Graphics Update" "External monitor disconnected"
  exit
fi
