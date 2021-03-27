#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Verify if we are using a laptop or desktop for choosing the bars
DMI_DESKTOP_ID=3
[ "$(cat /sys/class/dmi/id/chassis_type)" -ne ${DMI_DESKTOP_ID} ] && echo IS_LAPTOP=1 || IS_LAPTOP=0

# Get the primary and secondary monitors
PRIMARY_MONITOR=$(xrandr | grep " connected primary" | cut -f1 -d' ')
SECONDARY_MONITOR=$(xrandr | grep " connected" | grep -v "primary" | cut -f1 -d' ')

if [ ${IS_LAPTOP} -eq 0 ]; then
  # We are in the Desktop land, use the default bars
  MONITOR=${PRIMARY_MONITOR} NET_IF=enp4s0 polybar -c ~/.config/polybar/config.ini --reload main &
  [ -n "${SECONDARY_MONITOR}" ] && MONITOR=${SECONDARY_MONITOR} NET_IF=enp4s0 polybar -c ~/.config/polybar/config.ini --reload secondary &
else
  # We are in the Laptop land, use the default bars
  MONITOR=${PRIMARY_MONITOR} NET_IF=wlo1 polybar -c ~/.config/polybar/config.ini --reload main_notebook &
  [ -n "${SECONDARY_MONITOR}" ] && MONITOR=${SECONDARY_MONITOR} NET_IF=wlo1 polybar -c ~/.config/polybar/config.ini --reload secondary &
fi

echo "Bars launched..."
