{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def, managerCheckCerts)
import Network.TLS (TLSCertificateUsage (CertificateUsageAccept))


-- Import all relevant handler modules here.
import Handler.Root
import Handler.Home
import Handler.Profile
import Handler.Follow
import Handler.News
import Handler.Mobile

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "OsojiPhoto" resourcesOsojiPhoto

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
-- withOsojiPhoto :: AppConfig DefaultEnv () -> Logger -> (Application -> IO ()) -> IO ()
-- withOsojiPhoto conf logger f = do
--     s <- staticSite
--     dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
--             $ either error return . Database.Persist.Base.loadConfig
--     Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
--                                  Database.Persist.Base.runPool dbconf (runMigration migrateAll) p
--                                  let h = OsojiPhoto conf logger s p
--                                  defaultRunner (f . logWare) h
--   where
-- #ifdef DEVELOPMENT
--     logWare = logHandleDev (\msg -> logBS logger msg >> flushLogger logger)
-- #else
--     logWare = logStdout
-- #endif

-- -- for yesod devel
-- withDevelAppPort :: Dynamic
-- withDevelAppPort = toDyn $ defaultDevelApp withOsojiPhoto


-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    let mgrSettings = def { managerCheckCerts = \ _ _ ->
                            return CertificateUsageAccept }
    manager <- newManager mgrSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    let foundation = OsojiPhoto conf setLogger s p manager dbconf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
