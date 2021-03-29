{-# OPTIONS -fno-warn-missing-signatures #-}

module Main where

import XMonad hiding (config)
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.UrgencyHook hiding (urgencyConfig)

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Util.Run (spawnPipe)

import qualified XMonad.Local.Action as Local
import qualified XMonad.Local.Applications as Applications
import qualified XMonad.Local.Keys as Local
import qualified XMonad.Local.Layout as Local
import qualified XMonad.Local.Log as Local
import qualified XMonad.Local.Startup as Startup
import qualified XMonad.Local.Theme as Local
import qualified XMonad.Local.Workspaces as Workspaces

config =
  ewmh
    def
      { terminal = Applications.terminal
      , layoutHook = Local.layoutHook
      , manageHook =
          namedScratchpadManageHook Workspaces.scratchPads <> Local.manageHook
      , handleEventHook = Local.handleEventHook
      , logHook = Local.logHook
      , workspaces = Workspaces.names
      , modMask = mod4Mask
      , keys = Local.keys
      , focusFollowsMouse = False
      , startupHook = Startup.startupHook
      } `additionalKeys`
  Local.additionalKeys

navConf :: Navigation2DConfig
navConf =
  def
    { defaultTiledNavigation = hybridOf sideNavigation centerNavigation
    , floatNavigation = centerNavigation
    , screenNavigation = lineNavigation
    , layoutNavigation = [("Full", centerNavigation)]
    , unmappedWindowRect = [("Full", singleWindowRect)]
    }

-- Main
main :: IO ()
main = do
  _ <- spawnPipe Applications.statusBar
  xmonad $
    ewmh $
    dynamicProjects Workspaces.projects $
    withUrgencyHookC urgencyStyle urgencyConfig $
    withNavigation2DConfig navConf $ Local.xmonadColors config
  where
    urgencyConfig = UrgencyConfig Focused Dont
    urgencyStyle = BorderUrgencyHook "#ff0000"
