{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleInstances,
    FlexibleContexts, CPP #-}

module Foundation
    ( OsojiPhoto (..)
    , OsojiPhotoMessage (..)
    , resourcesOsojiPhoto
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , Route(..)
    , OsojiPhotoRoute
--    , StaticRoute (..)
--    , AuthRoute (..)
    ) where

import Prelude
import Yesod hiding (Route)
import Yesod.Static
import qualified Yesod.Static as Static
import Settings.StaticFiles
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)

import Settings (widgetFile, Extra (..))

#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import Database.Persist.GenericSql
import qualified Database.Persist.Store
import Settings (widgetFile)
import Model
import GooglePlusOAuth (credsExtraToUserInfo, authGooglePlus)

import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Text.Coffee (coffeeFile)
import Data.Text (Text)

#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif




-- getMobileRootR

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data OsojiPhoto = OsojiPhoto
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
--    , connPool :: Database.Persist.Base.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "OsojiPhoto" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype OsojiPhotoRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route OsojiPhoto = OsojiPhotoRoute
-- * Creates the value resourcesOsojiPhoto which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- OsojiPhoto. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the OsojiPhotoRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "OsojiPhoto" $(parseRoutesFile "config/routes")

type OsojiPhotoRoute = Route OsojiPhoto

type Form x = Html -> MForm OsojiPhoto OsojiPhoto (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod OsojiPhoto where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
--            toWidgetHead $ $(coffeeFile "templates/coffee-test.coffee")
            $(widgetFile "default-layout")
--            $(widgetFile "normalize")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")


    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js
    isAuthorized HomeR _ = do
      mauth <- maybeAuth
      case mauth of
        Nothing -> return AuthenticationRequired
        Just (_) -> return Authorized -- (key, user)
    isAuthorized _ _ = return Authorized


-- How to run database actions.
instance YesodPersist OsojiPhoto where
    type YesodPersistBackend OsojiPhoto = SqlPersist
--    runDB f = liftIOHandler
--            $ fmap connPool getYesod >>= Database.Persist.Base.runPool (undefined :: Settings.PersistConfig) f
    runDB f = do
      master <- getYesod
      Database.Persist.Store.runPool
                  (persistConfig master)
                  f   
                  (connPool master)


googleOAuth :: AuthPlugin OsojiPhoto
googleOAuth = authGooglePlus
              "783306877203.apps.googleusercontent.com" -- Consumer key
              "_DcM2RKo_ppnYUVX-a83Og-A" -- Consumer secret

instance YesodAuth OsojiPhoto where
    type AuthId OsojiPhoto = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR
    authHttpManager = httpManager
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just e -> return $ Just (entityKey e)
            Nothing -> do
                 case credsExtraToUserInfo (credsExtra creds) of
                   Just userInfo -> fmap Just $ insert $ userInfoToUser $ userInfo
                   _ -> return Nothing
-- User (credsIdent creds) Nothing



    -- You can add other plugins like BrowserID, email or OAuth here
--    authPlugins = [authOpenId]
--    authPlugins = [authGoogleEmail]
    authPlugins _ = [googleOAuth]

    renderAuthMessage _ _ _ = ""

-- Sends off your mail. Requires sendmail in production!
deliver :: OsojiPhoto -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage OsojiPhoto FormMessage where
    renderMessage _ _ = defaultFormMessage



