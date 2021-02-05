
{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module XMonad.Local.Startup
  ( startupHook,
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import XMonad hiding (startupHook)
import XMonad.Util.Cursor (setDefaultCursor)

startupHook :: X ()
startupHook = mconcat [spawnsHook, setFullscreenSupported]

spawnsHook :: X ()
spawnsHook = do
  spawn "xss-lock -l -- $HOME/.xmonad/scripts/lock.sh"
  spawn "$HOME/.xmonad/scripts/init-wallpaper.sh"
  spawn "nm-applet --no-agent"
  spawn "wal -R"
  spawn "xmodmap -e 'add mod4 = Menu'"
  setDefaultCursor xC_left_ptr

setFullscreenSupported :: X ()
setFullscreenSupported =
  addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- getAtom "_NET_SUPPORTED"
  newSupportedList <- mapM (fmap fromIntegral . getAtom) props
  io $ do
    supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
    changeProperty32
      dpy
      r
      a
      aTOM
      propModeReplace
      (nub $ newSupportedList ++ supportedList)
