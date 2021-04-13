{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Theme
  ( xmonadColors
  , tabConfig
  , promptConfig
  ) where

import qualified Colors as C
import XMonad
import XMonad.Layout.Tabbed
import XMonad.Local.Prompt (predicateFunction)
import XMonad.Prompt
import XMonad.Util.Themes

xmonadColors :: XConfig a -> XConfig a
xmonadColors x =
  x
    { normalBorderColor = C.background
    , focusedBorderColor = C.foreground
    , borderWidth = 2
    }

barTheme :: Theme
barTheme =
  def
    { activeColor = C.color8
    , inactiveColor = C.background
    , urgentColor = C.color6
    , activeBorderColor = C.color8
    , inactiveBorderColor = C.background
    , urgentBorderColor = C.color6
    , activeTextColor = C.foreground
    , inactiveTextColor = C.foreground
    , urgentTextColor = C.foreground
    , fontName = "xft:JetBrainsMono Nerd Font:style=Regular:size=9"
    , decoHeight = 22
    }

tabConfig :: Theme
tabConfig = barTheme

promptConfig :: XPConfig
promptConfig =
  def
    { position = CenteredAt (1 / 3) (1 / 2)
    , height = 50
    , font = "xft:JetBrainsMono Nerd Font:style=Medium:size=10"
    , bgColor = C.background
    , fgColor = C.foreground
    , fgHLight = C.color0
    , bgHLight = C.color1
    , borderColor = C.foreground
    , promptBorderWidth = 1
    , maxComplRows = Just 12
    , alwaysHighlight = False
    , promptKeymap = vimLikeXPKeymap
    , searchPredicate = predicateFunction
    }
