#!/usr/bin/env bash

# waits for wal to set the colors and then starts polybar and wal-set which launches dunst
while true; do
	if [[ ! $color15 ]]; then
		sh ~/scripts/wal-set.sh
		sh ~/.config/polybar/launch.sh
		exit 1
	fi
	sleep 0.1
done
