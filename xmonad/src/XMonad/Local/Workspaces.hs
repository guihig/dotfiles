{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}
--------------------------------------------------------------------------------
-- | Workspace configuration and utilities.
module XMonad.Local.Workspaces
  ( projects
  , terminal
  , names
  , scratchPads
  ) where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad hiding (terminal)
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.ManageHelpers (doFloatDep)
import XMonad.Layout.IndependentScreens
import XMonad.Local.Applications
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

workspaceNumbers = [show k | k <- [0 .. 4]]

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project
    {projectName = k, projectDirectory = "~/", projectStartHook = Nothing}
  | k <- workspaceNumbers
  ]

--------------------------------------------------------------------------------
-- | Names of my workspaces.
names :: [WorkspaceId]
names = withScreens 2 workspaceNumbers

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS
      { name = "spotify"
      , cmd = "spotify"
      , query = className =? "Spotify"
      , hook = forceBiggerCenterFloat
      }
  , NS
      { name = "slack"
      , cmd = "slack"
      , query = className =? "Slack"
      , hook = floatOnLeft
      }
  , NS
      { name = "joplin"
      , cmd = "joplin-desktop"
      , query = className =? "Joplin"
      , hook = forceBiggerCenterFloat
      }
  , NS
      { name = "mailspring"
      , cmd = "mailspring"
      , query = className =? "Mailspring"
      , hook = forceBiggerCenterFloat
      }
  , NS
      { name = "discord"
      , cmd = "discord"
      , query = className =? "discord"
      , hook = floatOnLeft
      }
  ]
  where
    floatOnRight =
      customFloating $ StackSet.RationalRect (3 / 5) (1 / 10) (2 / 5) (8 / 10)
    floatOnLeft =
      customFloating $ StackSet.RationalRect 0 (1 / 10) (1 / 2) (8 / 10)
    forceCenterFloat = doFloatDep move
      where
        move :: StackSet.RationalRect -> StackSet.RationalRect
        move _ = StackSet.RationalRect x y w h
        w, h, x, y :: Rational
        w = 1 / 2
        h = 1 / 2
        x = (1 - w) / 2
        y = (1 - h) / 2
    forceBiggerCenterFloat = doFloatDep move
      where
        move :: StackSet.RationalRect -> StackSet.RationalRect
        move _ = StackSet.RationalRect x y w h
        w, h, x, y :: Rational
        w = 3 / 4
        h = 3 / 4
        x = (1 - w) / 2
        y = (1 - h) / 2
