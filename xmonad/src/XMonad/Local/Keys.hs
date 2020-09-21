{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Keys
  ( keys,
    rawKeys,
    additionalKeys,
  )
where

--------------------------------------------------------------------------------
-- General Haskell Packages.
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath ((</>))
--------------------------------------------------------------------------------
-- Package: xmonad.
import XMonad hiding (keys)
--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.DynamicProjects (switchProjectPrompt)
--------------------------------------------------------------------------------
-- Local modules.

import XMonad.Actions.FloatKeys (keysResizeWindow)
import XMonad.Actions.Minimize
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
  ( onNextNeighbour,
    onPrevNeighbour,
  )
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.SwapPromote (swapHybrid)
import XMonad.Actions.TagWindows
  ( addTag,
    delTag,
    withTagged,
  )
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.IndependentScreens
  ( onCurrentScreen,
  )
import XMonad.Layout.Maximize (maximizeRestore)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
  ( GroupMsg (MergeAll, UnMerge),
    pullGroup,
    toSubl,
  )
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.Layout.ZoomRow
  ( zoomIn,
    zoomOut,
    zoomReset,
  )
import XMonad.Local.Applications as Applications
import XMonad.Local.Layout
  ( selectLayoutByName,
    toggleLayout,
  )
import qualified XMonad.Local.Theme as Local
import XMonad.Local.Workspaces (scratchPads)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window
  ( WindowPrompt (..),
    allWindows,
    windowMultiPrompt,
    wsWindows,
  )
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (namedScratchpadAction)

--------------------------------------------------------------------------------
-- Join all the key maps into a single list and send it through @mkKeymap@.
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys c = mkKeymap c (rawKeys c)

additionalKeys :: [((KeyMask, KeySym), X ())]
additionalKeys = workspaceKeys'

--------------------------------------------------------------------------------

-- | Access the unprocessed key meant to be fed into @mkKeymap@.
rawKeys :: XConfig Layout -> [(String, X ())]
rawKeys c = withUpdatePointer $ concatMap ($ c) keymaps
  where
    keymaps =
      [ baseKeys,
        windowKeys,
        workspaceKeys,
        layoutKeys,
        screenKeys,
        appKeys,
        systemKeys
      ]

--------------------------------------------------------------------------------

-- | Modify all keybindings so that after they finish their action the
-- mouse pointer is moved to the corner of the focused window.  This
-- is a bit of a hack to work around some issues I have with
-- @UpdatePointer@.
withUpdatePointer :: [(String, X ())] -> [(String, X ())]
withUpdatePointer = map addAction
  where
    addAction :: (String, X ()) -> (String, X ())
    addAction (key, action) = (key, action >> updatePointer (0.75, 0.25) (0, 0))

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
baseKeys :: XConfig Layout -> [(String, X ())]
baseKeys c =
  [ ("M-q", spawn "xmonad --restart"),
    ("M-C-q", spawn "xmonad --recompile && xmonad --restart"),
    ("M-S-q", confirmPrompt Local.promptConfig "Quit XMonad" $ io exitSuccess)
  ]

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
systemKeys :: XConfig Layout -> [(String, X ())]
systemKeys _ =
  [ ("S-<Print>", spawn "sleep 0.2 && gnome-screenshot -a"),
    ("M-v", spawn "polybar-msg cmd toggle"),
    ("M-=", spawn "$HOME/.xmonad/scripts/lock.sh")
  ]

--------------------------------------------------------------------------------
toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w $ W.floating s
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
    )

-- Window focusing, swapping, and other actions.
directions2D :: [Direction2D]
directions2D = [D, U, L, R]

arrowKeys :: [String]
arrowKeys = ["j", "k", "h", "l"]

data ResizeModes = RShrink | RExpand | RMirrorShrink | RMirrorExpand deriving (Show, Eq)

resizeHorizontal :: ResizeModes -> X ()
resizeHorizontal mode =
  withFocused
    ( \w -> do
        floats <- gets (W.floating . windowset)
        if M.member w floats
          then
            if mode == RShrink
              then keysResizeWindow (-10, 0) (0, 0) w
              else keysResizeWindow (10, 0) (0, 0) w
          else
            if mode == RShrink
              then sendMessage Shrink
              else sendMessage Expand
    )

resizeVertical :: ResizeModes -> X ()
resizeVertical mode =
  withFocused
    ( \w -> do
        floats <- gets (W.floating . windowset)
        if M.member w floats
          then
            if mode == RMirrorShrink
              then keysResizeWindow (0, 10) (0, 0) w
              else keysResizeWindow (0, -10) (0, 0) w
          else
            if mode == RMirrorShrink
              then sendMessage MirrorShrink
              else sendMessage MirrorExpand
    )

windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  -- Focusing Windows:
  [ ("M-,", windows W.focusUp),
    ("M-.", windows W.focusDown),
    ("M-S-,", windows W.swapUp),
    ("M-S-.", windows W.swapDown),
    ("M-u", focusUrgent),
    ("M-o", windowPromptGoto),
    ("M-C-m", windows W.focusMaster),
    ("M-[", resizeHorizontal RShrink),
    ("M-]", resizeHorizontal RExpand),
    ("M-S-[", resizeVertical RMirrorExpand),
    ("M-S-]", resizeVertical RMirrorShrink),
    ("M-w", kill1),
    ("M--", withFocused minimizeWindow >> windows W.focusDown),
    ("M-S--", withLastMinimized maximizeWindowAndFocus)
  ]
    ++
    -- Navigation Windows
    [("M-" ++ k, windowGo d True) | (k, d) <- zip arrowKeys directions2D]
    ++
    -- Navigation Windows Move
    [ ("M-C-" ++ k, windowSwap d False)
      | (k, d) <- zip arrowKeys directions2D
    ]

--------------------------------------------------------------------------------
-- Keys for manipulating workspaces.
workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys _ = [("M-p", switchProjectPrompt Local.promptConfig)]

workspaceKeys' :: [((KeyMask, KeySym), X ())]
workspaceKeys' =
  [ ((m .|. mod4Mask, k), windows $ onCurrentScreen f (show i))
    | (i, k) <- zip [0 .. 9] [xK_1 .. xK_9],
      (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]

--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys c =
  [ ("M-<Backspace>", selectLayoutByName Local.promptConfig),
    ( "M-f",
      sequence_ [withFocused $ windows . W.sink, sendMessage ToggleLayout]
    ),
    ("M-\\", toggleLayout "Tall"),
    ("M-t s", sendMessage ToggleStruts),
    ("M-y", withFocused toggleFloat),
    ("M-S-y", sinkAll),
    ("M-<Tab>", sendMessage NextLayout),
    ("M-S-<Tab>", toSubl NextLayout),
    ("M-g", withFocused (sendMessage . UnMerge)),
    ("M-S-g", withFocused (sendMessage . MergeAll))
  ]
    ++
    -- Sublayout Merge Windows
    [ ("M-S-C-" ++ k, sendMessage $ pullGroup d)
      | (k, d) <- zip arrowKeys directions2D
    ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-;", onNextNeighbour def W.view),
    ("M-S-;", onPrevNeighbour def W.view),
    ("M-C-;", screenSwap L True)
  ]
    ++ [ ("M-S-" ++ k, windowToScreen d False)
         | (k, d) <- zip arrowKeys directions2D
       ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys _c =
  [ ("M-<Return>", spawn Applications.terminal),
    ("M-<Esc>", shellPrompt Local.promptConfig),
    ("M-<Space>", spawn Applications.launcher),
    ("M-s", namedScratchpadAction scratchPads "spotify"),
    ("M-a", namedScratchpadAction scratchPads "slack"),
    ("M-d", namedScratchpadAction scratchPads "joplin"),
    ("M-r", namedScratchpadAction scratchPads "rocket.chat")
  ]

--------------------------------------------------------------------------------
windowPromptGoto :: X ()
windowPromptGoto = windowMultiPrompt Local.promptConfig modes
  where
    modes =
      [ (Goto, allWindows),
        (Goto, wsWindows),
        (BringCopy, allWindows),
        (Bring, allWindows)
      ]
