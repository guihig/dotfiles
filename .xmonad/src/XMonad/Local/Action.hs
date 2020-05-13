{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
-- | Utility functions dealing with XMonad events.
module XMonad.Local.Action
  ( manageHook
  , handleEventHook
  )
where

--------------------------------------------------------------------------------
import           Control.Monad                  ( when )
import qualified Data.Map                      as M
import           Data.Monoid
import           XMonad                  hiding ( manageHook
                                                , handleEventHook
                                                )
import           XMonad.Actions.TagWindows      ( addTag )
import           XMonad.Actions.SpawnOn         ( manageSpawn )
import           XMonad.Hooks.InsertPosition    ( Focus(..)
                                                , Position(..)
                                                , insertPosition
                                                )
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.ManageDocks       ( manageDocks
                                                , docksEventHook
                                                )
import           XMonad.Hooks.DynamicProperty   ( dynamicPropertyChange )
import qualified XMonad.StackSet               as W
import           XMonad.Layout.Fullscreen       ( fullscreenEventHook
                                                , fullscreenManageHook
                                                )
import           XMonad.Util.NamedScratchpad    ( customFloating )

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
manageHook :: ManageHook
manageHook =
  composeOne
      [
      -- Chrome debugging windows and application windows show up as
      -- pop-ups so we need to deal with that before floating pop-ups.
        (className
        =? "Chromium-browser"
        <&&> stringProperty "WM_WINDOW_ROLE"
        =? "pop-up"
        )
        -?> normalTile

      -- Force dialog windows and pop-ups to be floating.
      , stringProperty "WM_WINDOW_ROLE" =? "pop-up" -?> doCenterFloat
      , stringProperty "WM_WINDOW_ROLE" =? gtkFile -?> forceCenterFloat
      , className =? "Spotify" -?> forceCenterFloat
      , className =? "Slack" -?> forceCenterFloat
      , transience -- Move transient windows to their parent.
      , isDialog -?> doCenterFloat
      , isFullscreen -?> doFullFloat

      -- Tile all other windows using insertPosition.
      , pure True -?> normalTile
      ]
    <+> manageDocks
    <+> fullscreenManageHook
    <+> manageSpawn
 where
    -- | Sub-string match with a 'Query'.
  -- (=*) :: Query String -> String -> Query Bool
  -- (=*) q s = isInfixOf s <$> q

  gtkFile           = "GtkFileChooserDialog"
  normalTile        = insertPosition Above Newer
  _tileBelow        = insertPosition Below Newer
  _tileBelowNoFocus = insertPosition Below Older

--------------------------------------------------------------------------------
-- | If the given condition is 'True' then add the given tag name to
-- the window being mapped.  Always returns 'Nothing' to continue
-- processing other manage hooks.
_addTagAndContinue :: Query Bool -> String -> MaybeManageHook
_addTagAndContinue p tag = do
  x <- p
  when x (liftX . addTag tag =<< ask)
  return Nothing

--------------------------------------------------------------------------------
-- | Useful when a floating window requests stupid dimensions.  There
-- was a bug in Handbrake that would pop up the file dialog with
-- almost no height due to one of my rotated monitors.
forceCenterFloat :: ManageHook
forceCenterFloat = doFloatDep move
 where
  move :: W.RationalRect -> W.RationalRect
  move _ = W.RationalRect x y w h

  w, h, x, y :: Rational
  w = 1 / 2
  h = 1 / 2
  x = (1 - w) / 2
  y = (1 - h) / 2

--------------------------------------------------------------------------------
handleEventHook :: Event -> X All
handleEventHook = mconcat
  [ docksEventHook
  , focusFollowsTiledOnly
  , fullscreenEventHook
  , floatDynamicPropEventHook
  ]

--------------------------------------------------------------------------------
floatDynamicPropEventHook :: Event -> X All
floatDynamicPropEventHook = dynamicPropertyChange "WM_CLASS"
                                                  dynamicClassComposed

 where
  floatOnRight =
    customFloating $ W.RationalRect (3 / 5) (1 / 10) (2 / 5) (8 / 10)

  _floatOnLeft = customFloating $ W.RationalRect 0 (1 / 10) (1 / 2) (8 / 10)

  dynamicClassComposed = composeAll [className =? "Spotify" --> floatOnRight]

--------------------------------------------------------------------------------
-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
-- work you need to turn off 'focusFollowsMouse' in your configuration
-- and then add this function to your 'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@CrossingEvent { ev_window = w, ev_event_type = t }
  | isNormalEnter = whenX bothTiled (focus w) >> mempty
 where
  isNormalEnter  = t == enterNotify && ev_mode e == notifyNormal
  bothTiled      = notFloating w <&&> currentIsTiled
  currentIsTiled = currentWindow >>= maybe (return True) notFloating
  currentWindow  = gets $ W.peek . windowset
  notFloating w' = gets $ not . M.member w' . W.floating . windowset
focusFollowsTiledOnly _ = mempty
