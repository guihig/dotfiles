#!/usr/bin/env bash
if [ "$(pgrep -c -f "wtype-f-loop.sh")" -gt 1 ]; then
    pkill -f "wtype-f-loop.sh"
    exit 0
fi

sleep 0.3

while true; do
    DISPLAY=:1 xdotool key f
    sleep 1
done
