{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_diagrams_gtk_template (
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
version = Version [1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\bin"
libdir     = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\lib\\x86_64-windows-ghc-8.0.2\\diagrams-gtk-template-1.0"
dynlibdir  = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\share\\x86_64-windows-ghc-8.0.2\\diagrams-gtk-template-1.0"
libexecdir = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\libexec"
sysconfdir = "C:\\Users\\5630\\Google \12489\12521\12452\12502\\Haskell\\project\\diagrams-gtk-template\\.stack-work\\install\\65995373\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "diagrams_gtk_template_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "diagrams_gtk_template_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "diagrams_gtk_template_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "diagrams_gtk_template_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "diagrams_gtk_template_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "diagrams_gtk_template_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
