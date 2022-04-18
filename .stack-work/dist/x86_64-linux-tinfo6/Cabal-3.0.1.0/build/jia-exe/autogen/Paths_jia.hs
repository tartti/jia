{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_jia (
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

bindir     = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/bin"
libdir     = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/lib/x86_64-linux-ghc-8.8.4/jia-0.1.0.0-Hg29ZW9UsWXCPlj0UCTciF-jia-exe"
dynlibdir  = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/share/x86_64-linux-ghc-8.8.4/jia-0.1.0.0"
libexecdir = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/libexec/x86_64-linux-ghc-8.8.4/jia-0.1.0.0"
sysconfdir = "/home/tartti/git/jia/.stack-work/install/x86_64-linux-tinfo6/80915550621e61c51a845a51f175fc7ce7e4f05b27d95b61179b7fdcdc6eefcc/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "jia_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "jia_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "jia_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "jia_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "jia_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "jia_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
