{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pace2019 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/looleec/Library/Haskell/bin"
libdir     = "/Users/looleec/Library/Haskell/ghc-8.4.3-x86_64/lib/pace2019-0.1.0.0"
dynlibdir  = "/Users/looleec/Library/Haskell/ghc-8.4.3-x86_64/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/looleec/Library/Haskell/share/ghc-8.4.3-x86_64/pace2019-0.1.0.0"
libexecdir = "/Users/looleec/Library/Haskell/libexec/x86_64-osx-ghc-8.4.3/pace2019-0.1.0.0"
sysconfdir = "/Users/looleec/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pace2019_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pace2019_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pace2019_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pace2019_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pace2019_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pace2019_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
