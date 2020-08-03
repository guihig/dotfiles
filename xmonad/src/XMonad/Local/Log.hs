{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Log
  ( logHook,
  )
where

--------------------------------------------------------------------------------
import XMonad hiding (logHook)
import XMonad.Hooks.FadeInactive (isUnfocusedOnCurrentWS)
import XMonad.Hooks.FadeWindows
  ( FadeHook,
    fadeWindowsLogHook,
    isFloating,
    isUnfocused,
    opaque,
    transparency,
  )

logHook :: X ()
logHook = fadeWindowsLogHook fadeHook

fadeHook :: FadeHook
fadeHook =
  composeAll
    [ isUnfocused --> transparency 0.25,
      opaque,
      isUnfocusedOnCurrentWS --> transparency 0.25,
      isFloating --> transparency 0.0
    ]
