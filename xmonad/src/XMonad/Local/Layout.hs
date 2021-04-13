{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}
--------------------------------------------------------------------------------
-- | Layout configuration and hook.
module XMonad.Local.Layout
  ( layoutHook
  , selectLayoutByName
  , toggleLayout
  ) where

--------------------------------------------------------------------------------
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import XMonad hiding ((|||), float, layoutHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Accordion (Accordion(..))
import XMonad.Layout.BorderResize (borderResize)
import XMonad.Layout.Fullscreen (fullscreenFloat)
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.MultiToggle (mkToggle, single)
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Renamed (Rename(..), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.SubLayouts (subLayout)
import XMonad.Layout.Tabbed (addTabsAlways, shrinkText)
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Local.Prompt (aListCompFunc)
import XMonad.Local.Theme (tabConfig)
import XMonad.Prompt
import qualified XMonad.StackSet as Stack
import XMonad.Util.ExtensibleState as XState

--------------------------------------------------------------------------------
layoutHook =
  renamed [CutWordsLeft 2] $
  borderResize $
  minimize $ fullscreenFloat $ fullScreenToggle $ toggleLayouts full allLays
  where
    uniformBorder n = Border n n n n
    spacing = spacingRaw False (uniformBorder 0) True (uniformBorder 5) True
    tabs = avoidStruts $ addTabs' Simplest
    full = noBorders Full
    tall =
      avoidStruts $
      windowNavigation $
      addTabs' $ addSubLayout' $ spacing $ ResizableTall 1 (3 / 100) (2 / 3) []
    grid =
      avoidStruts $ windowNavigation $ addTabs' $ addSubLayout' $ spacing Grid
    addTabs' l = addTabsAlways shrinkText tabConfig l
    addSubLayout' = subLayout [] (Simplest ||| Accordion ||| Mirror Accordion)
    fullScreenToggle = mkToggle (single FULL)
    allLays =
      renamed [Replace "Tall"] tall |||
      renamed [Replace "Grid"] grid ||| renamed [Replace "Tabs"] tabs

--------------------------------------------------------------------------------
-- | A data type for the @XPrompt@ class.
data LayoutByName =
  LayoutByName

instance XPrompt LayoutByName where
  showXPrompt LayoutByName = "Layout: "

--------------------------------------------------------------------------------
-- | Use @Prompt@ to choose a layout.
selectLayoutByName :: XPConfig -> X ()
selectLayoutByName conf =
  mkXPrompt LayoutByName conf' (aListCompFunc conf' layoutNames) go
  where
    go :: String -> X ()
    go selected =
      case lookup selected layoutNames of
        Nothing -> return ()
        Just name -> sendMessage (JumpToLayout name)
    conf' :: XPConfig
    conf' = conf {alwaysHighlight = True}
    layoutNames :: [(String, String)]
    layoutNames = [("Grid", "Grid"), ("Tall", "Tall"), ("Tabs", "Tabs")]

--------------------------------------------------------------------------------
-- | Keep track of layouts when jumping with 'toggleLayout'.
newtype LayoutHistory =
  LayoutHistory
    { runLayoutHistory :: Map String String
    }
  deriving (Typeable)

instance ExtensionClass LayoutHistory where
  initialValue = LayoutHistory Map.empty

--------------------------------------------------------------------------------
-- | Toggle between the current layout and the one given as an argument.
toggleLayout :: String -> X ()
toggleLayout name = do
  winset <- XMonad.gets windowset
  let ws = Stack.workspace . Stack.current $ winset
      wn = Stack.tag ws
      ld = description . Stack.layout $ ws
  if name == ld
    then restoreLayout wn
    else rememberAndGo wn ld
    -- Restore the previous workspace.
  where
    restoreLayout :: String -> X ()
    restoreLayout ws = do
      history <- runLayoutHistory <$> XState.get
      let ld = fromMaybe "Tall" (Map.lookup ws history)
      sendMessage (JumpToLayout ld)
    -- Remember the current workspace and jump to the requested one.
    rememberAndGo :: String -> String -> X ()
    rememberAndGo ws current = do
      history <- runLayoutHistory <$> XState.get
      XState.put (LayoutHistory $ Map.insert ws current history)
      sendMessage (JumpToLayout name)
