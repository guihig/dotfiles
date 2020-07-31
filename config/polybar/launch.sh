#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar on multiple screens
if type "xrandr"; then
  for m in $(xrandr --query | grep -e " connected " | awk '/primary/' | cut -d" " -f1); do
    MONITOR=$m polybar --reload main -c ~/.config/polybar/config.ini 2>"$HOME/.config/polybar/main_error.txt" &
  done

  for m in $(xrandr --query | grep -e " connected " | awk '!/primary/' | cut -d" " -f1); do
    MONITOR=$m polybar --reload secondary -c ~/.config/polybar/config.ini 2>"$HOME/.config/polybar/secondary_error.txt" &
  done
fi

echo "Bars launched..."
