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
  ( projects,
    terminal,
    names,
    scratchPads,
  )
where

--------------------------------------------------------------------------------
import Control.Monad (unless)
import XMonad hiding (terminal)
import XMonad.Actions.DynamicProjects
import XMonad.Layout.IndependentScreens
import XMonad.Local.Applications
import qualified XMonad.StackSet as StackSet
import XMonad.Util.NamedScratchpad

worspaceNumbers = [show k | k <- [0 .. 4]]

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project
      { projectName = k,
        projectDirectory = "~/",
        projectStartHook = Nothing
      }
    | k <- worspaceNumbers
  ]

--------------------------------------------------------------------------------

-- | Names of my workspaces.
names :: [WorkspaceId]
names = withScreens 2 worspaceNumbers

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS
      { name = "spotify",
        cmd = "spotify",
        query = className =? "Spotify",
        hook = floatOnRight
      },
    NS
      { name = "slack",
        cmd = "slack",
        query = className =? "Slack",
        hook = floatOnLeft
      },
    NS
      { name = "joplin",
        cmd = "joplin-desktop",
        query = className =? "Joplin",
        hook = floatOnLeft
      },
    NS
      { name = "rocket.chat",
        cmd = "rocketchat-desktop",
        query = className =? "Rocket.Chat",
        hook = floatOnLeft
      }
  ]
  where
    floatOnRight =
      customFloating $ StackSet.RationalRect (3 / 5) (1 / 10) (2 / 5) (8 / 10)

    floatOnLeft =
      customFloating $ StackSet.RationalRect 0 (1 / 10) (1 / 2) (8 / 10)
