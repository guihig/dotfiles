{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Applications
    (statusBar, terminal, browser, launcher, spotify)
where

statusBar :: String
statusBar = "$HOME/.config/polybar/launch.sh"

terminal :: String
terminal = "alacritty"

browser :: String
browser = "firefox"

launcher :: String
launcher = "rofi -modi drun,run,ssh -show drun -matching fuzzy -show-icons -drun-icon-theme -lines 20 -padding 25 -width 30 -columns 1"

spotify :: String
spotify = "spotify"
