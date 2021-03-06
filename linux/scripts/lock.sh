#!/usr/bin/env bash

icon="/home/tinimini/Pictures/lock-icon.jpg"
tmpbg='/tmp/screen.png'

(( $# )) && { icon=$1; }

maim "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"

convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
i3lock -u -i "$tmpbg"
