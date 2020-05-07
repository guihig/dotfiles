#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

# Launch polybar on multiple screens
#if type "xrandr"; then
#for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
#    MONITOR=$m polybar --reload example &
# done
#else
  polybar --reload main 2> ${HOME}/.config/polybar/polybar_err.txt &
#fi

echo "Bars launched..."

