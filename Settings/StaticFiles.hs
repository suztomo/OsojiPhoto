{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Settings.StaticFiles where

import Prelude (IO)
--import Yesod.Static (staticFiles, StaticRoute (StaticRoute))
--import Yesod.Static (staticFiles, StaticRoute, Route (..))
--import qualified Yesod.Static as Static
--import Settings (staticDir)

import Yesod.Static
import qualified Yesod.Static as Static
import Settings (staticDir)

-- | use this to create your static file serving site
staticSite :: IO Static.Static
staticSite =
#ifdef DEVELOPMENT
  Static.staticDevel staticDir
#else
  Static.static staticDir
#endif

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)
