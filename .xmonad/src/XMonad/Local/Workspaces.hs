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
  , asKey
  , viewPrevWS
  )
where

--------------------------------------------------------------------------------
import           Control.Monad                  ( unless )
import           XMonad                  hiding ( terminal )
import           XMonad.Actions.DynamicProjects
import qualified XMonad.StackSet               as StackSet
import           XMonad.Util.NamedScratchpad
import           XMonad.Local.Applications
import           XMonad.Layout.IndependentScreens

--------------------------------------------------------------------------------
projects :: [Project]
projects =
  [ Project { projectName      = "GEN"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = "WRK"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = "WRK_2"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = "WRK_3"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  , Project { projectName      = "ETC"
            , projectDirectory = "~/"
            , projectStartHook = Nothing
            }
  ]

--------------------------------------------------------------------------------

-- | Names of my workspaces.
names :: [WorkspaceId]
names = withScreens 2 ["GEN", "WRK", "WRK_2", "WRK_3", "ETC"]

--------------------------------------------------------------------------------
scratchPads :: NamedScratchpads
scratchPads =
  [ NS { name  = "spotify"
       , cmd   = "spotify"
       , query = (className =? "Spotify")
       , hook  = floatOnRight
       }
  , NS { name  = "slack"
       , cmd   = "slack"
       , query = (className =? "Slack")
       , hook  = floatOnLeft
       }
  ]
 where
  floatOnRight =
    customFloating $ StackSet.RationalRect (2 / 3) (1 / 10) (1 / 3) (8 / 10)

  floatOnLeft =
    customFloating $ StackSet.RationalRect 0 (1 / 10) (1 / 2) (8 / 10)

--------------------------------------------------------------------------------

-- | Helper function to translate workspace names into key names for
-- using in @EZConfig@ key bindings.
asKey :: String -> String
asKey ('F' : xs) = "<F" ++ xs ++ ">" -- Function key (i.e. "F1" -> "<F1>")
asKey x          = x -- Any other key.

--------------------------------------------------------------------------------

-- | Toggle between the current and previous workspaces.
viewPrevWS :: X ()
viewPrevWS = do
  ws <- gets windowset
  let hs = filter (\w -> StackSet.tag w /= "NSP") $ StackSet.hidden ws
  unless (null hs) (windows . StackSet.view . StackSet.tag $ head hs)
