#!/usr/bin/env bash

DEVICE="Touchpad"

devlist=$(xinput --list | grep "$DEVICE" | sed -n 's/.*id=\([0-9]\+\).*/\1/p')

for dev in $devlist
do
  xinput set-prop $dev "libinput Tapping Enabled" 1
  xinput set-prop $dev "libinput Natural Scrolling Enabled" 1
  xinput set-prop $dev "libinput Click Method Enabled" {1 1}
done

