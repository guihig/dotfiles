{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Theme
       ( xmonadColors
       , tabConfig
       )
where

import           XMonad
import           XMonad.Util.Themes
import           XMonad.Layout.Tabbed

xmonadColors :: XConfig a -> XConfig a
xmonadColors x = x { normalBorderColor  = "#444444"
                   , focusedBorderColor = "#00e8c6"
                   , borderWidth        = 2
                   }


tabConfig = (theme adwaitaDarkTheme) { fontName   = "xft:Terminus:size=12"
                                     , decoHeight = 20
                                     }
