{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_xmonadrc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [19,5,4] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ferreira/.cabal/bin"
libdir     = "/home/ferreira/.cabal/lib/x86_64-linux-ghc-8.6.5/xmonadrc-19.5.4-inplace-checkrc"
dynlibdir  = "/home/ferreira/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/ferreira/.cabal/share/x86_64-linux-ghc-8.6.5/xmonadrc-19.5.4"
libexecdir = "/home/ferreira/.cabal/libexec/x86_64-linux-ghc-8.6.5/xmonadrc-19.5.4"
sysconfdir = "/home/ferreira/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "xmonadrc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "xmonadrc_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "xmonadrc_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "xmonadrc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xmonadrc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xmonadrc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
