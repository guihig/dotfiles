{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Theme
  ( xmonadColors,
    tabConfig,
    topBarConfig,
    promptConfig,
  )
where

import qualified Colors as C
import XMonad
import XMonad.Layout.Tabbed
import XMonad.Local.Prompt (predicateFunction)
import XMonad.Prompt
import XMonad.Util.Themes

xmonadColors :: XConfig a -> XConfig a
xmonadColors x =
  x
    { normalBorderColor = C.background,
      focusedBorderColor = C.foreground,
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

promptConfig :: XPConfig
promptConfig =
  def
    { position = CenteredAt (1 / 3) (1 / 2),
      height = 50,
      font = "xft:Iosevka Nerd Font:style=Medium:size=12",
      bgColor = C.background,
      fgColor = C.foreground,
      fgHLight = C.color0,
      bgHLight = C.color1,
      borderColor = C.foreground,
      promptBorderWidth = 1,
      maxComplRows = Just 12,
      alwaysHighlight = False,
      promptKeymap = vimLikeXPKeymap,
      searchPredicate = predicateFunction
    }
