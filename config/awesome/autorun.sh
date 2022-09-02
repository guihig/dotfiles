#!/usr/bin/env bash

# https://github.com/raven2cz/awesomewm-config/blob/master/autorun.sh
function run {
  if ! pgrep -f $1 ;
  then
    $@&
  fi
}

# run "$HOME/.config/polybar/launch.sh"
run wal -R
run picom
run nm-applet --no-agent
