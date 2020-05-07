module XMonad.Local.Startup
    ( startupHook
    , setFullscreenSupported
    )
where

import           Control.Monad
import           Data.Maybe
import           Data.List
import           XMonad                  hiding ( startupHook )
import           XMonad.Util.Cursor             ( setDefaultCursor )

startupHook :: X ()
startupHook = do
    spawn "$HOME/.xmonad/init-wallpaper.sh"
    setDefaultCursor xC_left_ptr

setFullscreenSupported :: X ()
setFullscreenSupported =
    addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r                <- asks theRoot
    a                <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
        supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
        changeProperty32 dpy
                         r
                         a
                         aTOM
                         propModeReplace
                         (nub $ newSupportedList ++ supportedList)
