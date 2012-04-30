module Paths_OsojiPhoto (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/suztomo/Library/Haskell/ghc-7.0.3/lib/OsojiPhoto-0.0.0/bin"
libdir     = "/Users/suztomo/Library/Haskell/ghc-7.0.3/lib/OsojiPhoto-0.0.0/lib"
datadir    = "/Users/suztomo/Library/Haskell/ghc-7.0.3/lib/OsojiPhoto-0.0.0/share"
libexecdir = "/Users/suztomo/Library/Haskell/ghc-7.0.3/lib/OsojiPhoto-0.0.0/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "OsojiPhoto_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "OsojiPhoto_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "OsojiPhoto_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "OsojiPhoto_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
