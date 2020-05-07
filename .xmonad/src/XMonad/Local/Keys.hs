--------------------------------------------------------------------------------
{- This file is part of the xmonadrc package. It is subject to the
license terms in the LICENSE file found in the top-level directory of
this distribution and at git://pmade.com/xmonadrc/LICENSE. No part of
the xmonadrc package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file. -}

--------------------------------------------------------------------------------

-- | Key bindings.
module XMonad.Local.Keys
  ( keys
  , rawKeys
  , additionalKeys
  )
where

--------------------------------------------------------------------------------
-- General Haskell Packages.
import qualified Data.Map                      as M
import           Graphics.X11.Xlib
import           System.Directory
import           System.FilePath                ( (</>) )
import           System.Exit                    ( exitWith
                                                , ExitCode(ExitSuccess)
                                                )
--------------------------------------------------------------------------------
-- Package: xmonad.
import           XMonad                  hiding ( keys )
--------------------------------------------------------------------------------
-- Package: xmonad-contrib.
import           XMonad.Actions.CopyWindow      ( kill1 )
import           XMonad.Actions.DynamicProjects ( switchProjectPrompt )
import           XMonad.Actions.Minimize
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.PhysicalScreens ( onNextNeighbour
                                                , onPrevNeighbour
                                                )
import           XMonad.Actions.Promote         ( promote )
import           XMonad.Actions.SwapPromote     ( swapHybrid )
import           XMonad.Actions.TagWindows      ( addTag
                                                , delTag
                                                , withTagged
                                                )
import           XMonad.Actions.UpdatePointer   ( updatePointer )
import           XMonad.Actions.SinkAll         ( sinkAll )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(..) )
import           XMonad.Hooks.UrgencyHook       ( focusUrgent )
import           XMonad.Layout.LayoutBuilder    ( IncLayoutN(..) )
import           XMonad.Layout.Maximize         ( maximizeRestore )
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ZoomRow          ( zoomIn
                                                , zoomOut
                                                , zoomReset
                                                )
import           XMonad.Layout.SubLayouts       ( toSubl
                                                , pullGroup
                                                , GroupMsg(UnMerge)
                                                , GroupMsg(MergeAll)
                                                )
import           XMonad.Layout.IndependentScreens
                                                ( onCurrentScreen )
import           XMonad.Layout.ToggleLayouts    ( ToggleLayout(..) )
--------------------------------------------------------------------------------
-- Local modules.
import           XMonad.Local.Layout            ( selectLayoutByName
                                                , toggleLayout
                                                )
import           XMonad.Local.Layout.Columns    ( IncMasterCol(..) )
import qualified XMonad.Local.Prompt           as Local
import           XMonad.Local.Tagging
import           XMonad.Local.Workspaces        ( asKey
                                                , scratchPads
                                                )
import           XMonad.Prompt
import           XMonad.Prompt.Shell            ( shellPrompt )
import           XMonad.Prompt.Window           ( WindowPrompt(..)
                                                , allWindows
                                                , windowMultiPrompt
                                                , wsWindows
                                                )
import           XMonad.Prompt.ConfirmPrompt    ( confirmPrompt )
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( mkKeymap )
import qualified XMonad.Util.ExtensibleState   as XState
import           XMonad.Util.NamedScratchpad    ( namedScratchpadAction )
import           XMonad.Local.Applications     as Applications

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
    [ baseKeys
    , windowKeys
    , windowTagKeys
    , workspaceKeys
    , layoutKeys
    , screenKeys
    , appKeys
    , systemKeys
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
  [ ("M-x r", restartIntoDebugging)
  , ("M-q"  , spawn "xmonad --restart")
  , ("M-C-q", spawn "xmonad --recompile && xmonad --restart")
  , ( "M-S-q"
    , confirmPrompt Local.promptConfig "Quit XMonad" $ io (exitWith ExitSuccess)
    )
  , ("M-x <Space>", messageMenu c Local.promptConfig)
  , ("M-*"        , repeatLastXMessage)
  , ("C-A-l"      , spawn "xdg-screensaver lock")
  ]

--------------------------------------------------------------------------------
-- Specifically manage my prefix key (C-z), and for controlling XMonad.
systemKeys :: XConfig Layout -> [(String, X ())]
systemKeys _ =
  [ ("S-<Print>", spawn "gnome-screenshot -a")
  , ("M-v"      , spawn "polybar-msg cmd toggle")
  ]

--------------------------------------------------------------------------------
toggleFloat :: Window -> X ()
toggleFloat w = windows
  (\s -> if M.member w $ W.floating s
    then W.sink w s
    else (W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s)
  )

-- Window focusing, swapping, and other actions.
directions2D :: [Direction2D]
directions2D = [D, U, L, R]

arrowKeys :: [String]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]

windowKeys :: XConfig Layout -> [(String, X ())]
windowKeys _ =
  -- Focusing Windows:
  [ ("M-;"            , windows W.focusUp)
    , ("M-S-;"          , windows W.focusDown)
    , ("M-u"            , focusUrgent)
    , ("M-o"            , windowPromptGoto)
    , ("M-C-m"          , windows W.focusMaster)
    , ("M-S-p"          , whenX (swapHybrid False) promote)
    , -- Promote current window to master.

    -- Resizing Windows:
      ("M-["            , sendMessage Shrink)
    , ("M-]"            , sendMessage Expand)
    , ("M-S-["          , sendMessage MirrorShrink)
    , ("M-S-]"          , sendMessage MirrorExpand)
    ,

    -- Window Layers and Killing and Yanking:
      ("M-<Delete>"     , kill1)
    , -- Kill the current window.
      ("M-<KP_Subtract>", withFocused minimizeWindow >> windows W.focusDown)
    , ("M-<KP_Add>", withLastMinimized maximizeWindowAndFocus)
    ]
    ++
  -- Navigation Windows
       [ ("M-" ++ k, windowGo d False) | (k, d) <- zip arrowKeys directions2D ]
    ++
  -- Navigation Windows Move
       [ ("M-C-" ++ k, windowSwap d False)
       | (k, d) <- zip arrowKeys directions2D
       ]
--------------------------------------------------------------------------------
-- Navigate windows by using tags.
windowTagKeys :: XConfig Layout -> [(String, X ())]
windowTagKeys _ =
  [ ("M-/", tagPrompt Local.promptConfig >> sendMessage (IncLayoutN 0))
    , ("M-j"  , primaryJumpTagUp)
    , ("M-C-j", secondaryJumpTagUp)
    , ("M-t a", addFocusTag)
    , ("M-t d", rmFocusTag)
    , ("M-t j", tagPrompt' Local.promptConfig [SetJumpTag])
    , ("M-t r", rmFocusTagAll >> addFocusTag)
    ]
    ++ numberedTags

 where
  addFocusTag :: X ()
  addFocusTag = do
    withFocused (addTag "focus")
    sendMessage (IncLayoutN 0)
  rmFocusTag :: X ()
  rmFocusTag = do
    withFocused (delTag "focus")
    sendMessage (IncLayoutN 0)
  rmFocusTagAll :: X ()
  rmFocusTagAll = withTagged "focus" (delTag "focus")
  numberedTags :: [(String, X ())]
  numberedTags = do
    key <- map show ([0 .. 9] :: [Int])
      ++ map (("F" ++) . show) ([1 .. 12] :: [Int])
    (prefix, action) <- numberedTemplate
    return (prefix ++ asKey key, action key)
  numberedTemplate :: [(String, String -> X ())]
  numberedTemplate = [("M-", focusTag'), ("M-C-", toggleTagOnCurrentWindow)]

--------------------------------------------------------------------------------
-- Keys for manipulating workspaces.
workspaceKeys :: XConfig Layout -> [(String, X ())]
workspaceKeys _ = [("M-p", switchProjectPrompt Local.promptConfig)]
  -- ("M-a", viewPrevWS)
    -- ++
  -- View WS
      --  [ ("M-" ++ k, windows $ onCurrentScreen W.view w)
      --  | (w, k) <- zip (workspaces c) wsKeys
      --  ]
--  where
--   wsKeys :: [String]
--   wsKeys = map show ([1 .. 9] :: [Int])
  --   ++
  -- -- Move to WS
  --      [ (("M-S-" ++ k), windows $ onCurrentScreen W.shift w)
  --      | (w, k) <- zip (workspaces c) wsKeys
  --      ]

workspaceKeys' :: [((KeyMask, KeySym), X ())]
workspaceKeys' =
  [ ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
  | (i, k) <- zip ["GEN", "WRK", "WRK_2", "WRK_3", "ETC"] [xK_1 .. xK_9]
  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]


--------------------------------------------------------------------------------
-- Layout switching and manipulation.
layoutKeys :: XConfig Layout -> [(String, X ())]
layoutKeys c =
  [ ("M-<Backspace>", selectLayoutByName Local.promptConfig)
    , ("M-w <Esc>"    , setLayout (layoutHook c))
    , -- Reset to default layout.
      ("M-`"          , withFocused (sendMessage . maximizeRestore))
    , ( "M-f"
      , sequence_ [withFocused $ windows . W.sink, sendMessage ToggleLayout]
      )
    , ("M-\\"     , toggleLayout "Tall")
    , ("M-."      , sendMessage (IncMasterN 1))
    , ("M-,"      , sendMessage (IncMasterN (-1)))
    , ("M-S-."    , sendMessage (IncLayoutN 1))
    , ("M-S-,"    , sendMessage (IncLayoutN (-1)))
    , ("M-C-."    , sendMessage (IncMasterCol 1))
    , ("M-C-,"    , sendMessage (IncMasterCol (-1)))
    , ("M-w s"    , sendMessage ToggleStruts)
    , ("M-y"      , withFocused toggleFloat)
    , -- Toogle tile and Float.
      ("M-S-y"    , sinkAll)
    , -- Toogle tile and Float.
      ("M-<Tab>"  , sendMessage NextLayout)
    , ("M-S-<Tab>", toSubl NextLayout)
    , ("M-g"      , withFocused (sendMessage . UnMerge))
    , ("M-S-g"    , withFocused (sendMessage . MergeAll))
    ]
    ++
  -- Sublayout Merge Windows
       [ (("M-S-C-" ++ k), sendMessage $ pullGroup d)
       | (k, d) <- zip arrowKeys directions2D
       ]

--------------------------------------------------------------------------------
-- Keys to manipulate screens (actual physical monitors).
screenKeys :: XConfig Layout -> [(String, X ())]
screenKeys _ =
  [ ("M-h", onNextNeighbour def W.view)
    , ("M-l", onPrevNeighbour def W.view)
    , ("M-=", screenSwap L True)
    ]
    ++ [ (("M-S-" ++ k), windowToScreen d False)
       | (k, d) <- zip arrowKeys directions2D
       ]

--------------------------------------------------------------------------------
-- Keys for launching applications.
appKeys :: XConfig Layout -> [(String, X ())]
appKeys _c =
  [ ("M-<Return>", spawn Applications.terminal)
  , ("M-<Esc>"   , shellPrompt Local.promptConfig)
  , ("M-<Space>" , spawn Applications.launcher)
  , ("M-s"       , namedScratchpadAction scratchPads "spotify")
  , ("M-a"       , namedScratchpadAction scratchPads "slack")
  ]

--------------------------------------------------------------------------------

-- | Restart XMonad but instead of starting the XMonad in @PATH@,
-- start a debugging version built out of my development tree.
restartIntoDebugging :: X ()
restartIntoDebugging = do
  home <- io getHomeDirectory
  restart (home </> "src/rc/xmonadrc/result/bin/xmonadrc") True

--------------------------------------------------------------------------------
windowPromptGoto :: X ()
windowPromptGoto = windowMultiPrompt Local.promptConfig modes
 where
  modes =
    [ (Goto     , allWindows)
    , (Goto     , wsWindows)
    , (BringCopy, allWindows)
    , (Bring    , allWindows)
    ]

--------------------------------------------------------------------------------

-- | A menu of less frequently used actions:
data MessageMenu = MessageMenu

instance XPrompt MessageMenu where
  showXPrompt MessageMenu = "XMonad Action: "

messageMenu :: XConfig Layout -> XPConfig -> X ()
messageMenu xc conf = mkXPrompt MessageMenu
                                conf'
                                (Local.aListCompFunc conf' actions)
                                go
 where
  go :: String -> X ()
  go selected = maybe (return ()) recordXMessage $ lookup selected actions
  conf' :: XPConfig
  conf' = conf { alwaysHighlight = True }
  actions :: [(String, X ())]
  actions =
    [ ("IncLayoutN"       , sendMessage (IncLayoutN 1))
    , ("DecLayoutN"       , sendMessage (IncLayoutN (-1)))
    , ("Next Layout"      , sendMessage NextLayout)
    , ("IncMasterN"       , sendMessage (IncMasterN 1))
    , ("DecMasterN"       , sendMessage (IncMasterN (-1)))
    , ("ToggleStruts"     , sendMessage ToggleStruts)
    , ("ToggleSpacing"    , toggleWindowSpacingEnabled)
    , ("Tile Window"      , withFocused $ windows . W.sink)
    , ("Screen Spacing 0" , setScreenSpacing (Border 0 0 0 0))
    , ("Screen Spacing +5", incWindowSpacing 5)
    , ("Screen Spacing -5", decWindowSpacing 5)
    , ("Window Spacing +5", incWindowSpacing 5)
    , ("Window Spacing -5", decWindowSpacing 5)
    , ("ZoomIn"           , sendMessage zoomIn)
    , ("ZoomOut"          , sendMessage zoomOut)
    , ("ZoomReset"        , sendMessage zoomReset)
    , ("Reset Layout"     , setLayout (layoutHook xc))
    ]

--------------------------------------------------------------------------------

-- | Remember certain actions taken so they can be repeated.
newtype LastXMessage = LastXMessage
  {getLastMessage :: X ()}

instance ExtensionClass LastXMessage where
  initialValue = LastXMessage (return ())

--------------------------------------------------------------------------------

-- | Record the given message as the last used message, then execute it.
recordXMessage :: X () -> X ()
recordXMessage message = do
  XState.put (LastXMessage message)
  message

--------------------------------------------------------------------------------

-- | Execute the last recorded message.
repeatLastXMessage :: X ()
repeatLastXMessage = getLastMessage =<< XState.get
