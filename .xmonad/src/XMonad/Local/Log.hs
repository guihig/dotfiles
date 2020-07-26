{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Log
  ( logHook
  )
where

--------------------------------------------------------------------------------
import           XMonad                  hiding ( logHook )
import           XMonad.Hooks.FadeWindows       ( FadeHook
                                                , fadeWindowsLogHook
                                                , transparency
                                                , opaque
                                                , isFloating
                                                )
import           XMonad.Hooks.FadeInactive      ( isUnfocusedOnCurrentWS )


logHook :: X ()
logHook = fadeWindowsLogHook fadeHook

fadeHook :: FadeHook
fadeHook = composeAll
  [ opaque
  , isUnfocusedOnCurrentWS --> transparency 0.25
  , isFloating --> transparency 0.0
  ]
