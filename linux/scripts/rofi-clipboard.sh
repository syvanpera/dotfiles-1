#!/usr/bin/env bash

~/.dotfiles/bin/clipster -o -n 0 -0 | rofi -i -dmenu -sep '\x00' -p clipboard: | sed -ze 's/\n$//' | ~/.dotfiles/bin/clipster
