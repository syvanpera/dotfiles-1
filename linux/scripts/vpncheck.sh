#!/bin/bash

if ps aux | rg "[o]penconnect" >&/dev/null; then
    echo %{F#A3BE8C}%{F-} on;
else
    echo %{F#A3BE8C}%{F-} off;
fi
