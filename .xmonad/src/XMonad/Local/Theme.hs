{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Theme
  ( xmonadColors,
    tabConfig,
    topBarConfig,
  )
where

import XMonad
import XMonad.Layout.Tabbed
import XMonad.Util.Themes

xmonadColors :: XConfig a -> XConfig a
xmonadColors x =
  x
    { normalBorderColor = "#444444",
      focusedBorderColor = "#dddddd",
      borderWidth = 1
    }

tabConfig =
  (theme adwaitaDarkTheme)
    { fontName = "xft:Terminus:size=12",
      decoHeight = 20
    }

topBarConfig :: Theme
topBarConfig =
  (theme adwaitaDarkTheme)
    { fontName = "xft:Terminus:size=12",
      decoHeight = 20
    }
