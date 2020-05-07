{-# OPTIONS -fno-warn-missing-signatures #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import           XMonad                  hiding ( config )
import           XMonad.Actions.DynamicProjects ( dynamicProjects )
import           XMonad.Actions.Navigation2D
import           XMonad.Config.Desktop          ( desktopConfig )
import           XMonad.Hooks.UrgencyHook
                                         hiding ( urgencyConfig )
import           XMonad.Util.NamedScratchpad    ( namedScratchpadManageHook )
import           XMonad.Hooks.FadeWindows       ( fadeWindowsEventHook )
import           XMonad.Hooks.EwmhDesktops      ( ewmh )

--------------------------------------------------------------------------------
import qualified XMonad.Local.Action           as Local
import qualified XMonad.Local.Keys             as Local
import qualified XMonad.Local.Layout           as Local
import qualified XMonad.Local.Log              as Local
import qualified XMonad.Local.Theme            as Local
import qualified XMonad.Local.Workspaces       as Workspaces
import qualified XMonad.Local.Applications     as Applications
import qualified XMonad.Local.Startup          as Startup
import           XMonad.Util.EZConfig           ( additionalKeys )
import           XMonad.Util.Run                ( spawnPipe )

--------------------------------------------------------------------------------
-- Damn you XMonad and your crazy type signatures!
--
-- config :: XConfig a
config =
  desktopConfig
      { terminal          = Applications.terminal
      , layoutHook        = Local.layoutHook
      , manageHook        = manageHook desktopConfig
                            <> namedScratchpadManageHook Workspaces.scratchPads
                            <> Local.manageHook
      , handleEventHook   = handleEventHook desktopConfig
                            <> fadeWindowsEventHook
                            <> Local.handleEventHook
      , logHook           = logHook desktopConfig <> Local.logHook
      , workspaces        = Workspaces.names
      , modMask           = mod4Mask
      , keys              = Local.keys
      , focusFollowsMouse = False
      , startupHook = Startup.startupHook <> Startup.setFullscreenSupported
      }
    `additionalKeys` Local.additionalKeys

--------------------------------------------------------------------------------
-- | Configuration for 'XMonad.Actions.Navigation2D'.
navConf :: Navigation2DConfig
navConf = def
  { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
  , floatNavigation        = centerNavigation
  , screenNavigation       = lineNavigation
  , layoutNavigation       = [("Full", centerNavigation)]
  , unmappedWindowRect     = [("Full", singleWindowRect)]
  }


--------------------------------------------------------------------------------
main :: IO ()
main = do
  _ <- spawnPipe Applications.statusBar
  xmonad
    $ dynamicProjects Workspaces.projects
    $ withUrgencyHookC urgencyStyle urgencyConfig
    $ ewmh
    $ withNavigation2DConfig navConf
    $ Local.xmonadColors config
 where
  urgencyConfig = UrgencyConfig Focused Dont
  urgencyStyle  = BorderUrgencyHook "#ff0000"
