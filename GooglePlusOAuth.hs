{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module GooglePlusOAuth
    ( oauthUrl
    , authGooglePlus
    , googlePlusUrl
    , GoogleUserInfo (..)
    , decodeUserInfo
    , credsExtraToUserInfo
    ) where


import Prelude (map, ($), String, Bool(..), lookup, putStrLn, IO, (++), show, (.),length, (>), (<), Show, Eq)
import Yesod.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Data.Either
import Text.Hamlet (shamlet)


import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Arrow ((***))
import Control.Monad.IO.Class (liftIO)
import Control.Monad
--import Control.Monad.Trans.Class (lift)
import Data.Text (Text, unpack, append)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8With, decodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.Encoding as DTE
import Control.Applicative ((<$>), (<*>))
import Network.TLS
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Control.Exception as X
import Control.Exception.Lifted as L
import Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString.Lazy as AP (Result(..),parse,eitherResult)
import Network.HTTP.Types
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (catMaybes)
import qualified Data.Map as M

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]


data AccessToken = AccessToken {
      token :: Text
} deriving (Show, Eq)
instance FromJSON AccessToken where
    parseJSON (Object o) = do
      t <- parseJSON =<< (o .: "access_token")
      return $ AccessToken t
decodeAccessToken :: LBS.ByteString -> Maybe AccessToken
decodeAccessToken input = decode input

data GoogleUserInfo = GoogleUserInfo {
      userInfoId :: Text,
      userInfoEmail :: Text,
      userInfoName :: Text,
      userInfoGivenName :: Text,
      userInfoFamilyName :: Text,
      userInfoLink :: Text,
      userInfoPictureURL :: Text,
      userInfoGender :: Text,
      userInfoLocale :: Text
} deriving (Show, Eq)

instance FromJSON GoogleUserInfo where
    parseJSON (Object o) = GoogleUserInfo <$>
                           o .: "id" <*>
                           o .: "email" <*>
                           o .: "name" <*>
                           o .: "given_name" <*>
                           o .: "family_name" <*>
                           o .: "link" <*>
                           o .: "picture" <*>
                           o .: "gender" <*>
                           o .: "locale"
    parseJSON _ = mzero

userInfoToCredsExtra :: GoogleUserInfo -> [(Text, Text)]
userInfoToCredsExtra i = [
 ("id", (userInfoId i)),
 ("email", (userInfoEmail i)),
 ("name", (userInfoName i)),
 ("given_name", (userInfoFamilyName i)),
 ("family_name", (userInfoGivenName i)),
 ("link", (userInfoLink i)),
 ("picture", (userInfoPictureURL i)),
 ("gender", (userInfoGender i)),
 ("locale", (userInfoLocale i)) ]




credsExtraToUserInfo :: [(Text, Text)] -> Maybe GoogleUserInfo
credsExtraToUserInfo lst = GoogleUserInfo <$>
                           lookup "id" lst <*>
                           lookup "email" lst <*>
                           lookup "name" lst <*>
                           lookup "given_name" lst <*>
                           lookup "family_name" lst <*>
                           lookup "link" lst <*>
                           lookup "picture" lst <*>
                           lookup "gender" lst <*>
                           lookup "locale" lst

decodeUserInfo :: LBS.ByteString -> Maybe GoogleUserInfo
decodeUserInfo input = decode input


accessOAuthData :: Manager -> String ->
                   [(BS.ByteString, BS.ByteString)] -> IO (Maybe LBS.ByteString)
accessOAuthData mgr url payload = do
    parsedUrl <- parseUrl url
    let req = if length payload > 0
              then urlEncodedBody payload $ parsedUrl -- POST
              else parsedUrl -- GET
        errHandler :: ResourceIO m => HttpException
                   -> ResourceT m (Response LBS.ByteString)
        errHandler (StatusCodeException status hdrs) = return $ Response status hdrs LBS.empty
    (status, content) <- C.runResourceT $ do
        Response st _ bsrc <- (httpLbs req mgr) `L.catch` errHandler
        return (st, bsrc)
    case status of
      (Status 200 _) -> return $ Just content
      a -> do
        putStrLn $ "invalid status: " ++ (show a)
        return Nothing

authGoogleOAuth :: YesodAuth m =>
             Text -- ^ Service Name
          -> String -- ^ OAuth Parameter Name to use for identify
          -> Text   -- ^ Request URL
          -> String -- ^ Access Token URL
          -> String -- ^ Authorize URL
          -> String -- ^ Consumer Key
          -> String -- ^ Consumer Secret
          -> AuthPlugin m
authGoogleOAuth name ident reqUrl accUrl authUrl key sec = AuthPlugin name dispatch login
  where
    url = PluginR name []
    getUserInfo :: Manager -> Text -> IO (Maybe LBS.ByteString)
    getUserInfo mgr accessToken = do
      accessOAuthData mgr targetUrl []
        where targetUrl = "https://www.googleapis.com/oauth2/v1/userinfo?access_token=" ++ (unpack accessToken)

    getAccessToken :: Manager -> Text -> String -> IO (Maybe Text)
    getAccessToken mgr callback code = do
      let payload = [("code", C8.pack code), ("client_id", C8.pack key),
                     ("client_secret", C8.pack sec),
                     ("redirect_uri", DTE.encodeUtf8 callback),
                     ("grant_type", "authorization_code")]
      parsedUrl <- parseUrl accUrl
      let req = urlEncodedBody payload $ parsedUrl
          errHandler :: ResourceIO m => HttpException
                     -> ResourceT m (Response LBS.ByteString)
          errHandler (StatusCodeException status hdrs) = return $ Response status hdrs LBS.empty
      (status, content) <- C.runResourceT $ do
        Response st _ bsrc <- (httpLbs req mgr) `L.catch` errHandler
        return (st, bsrc)

--      (status, content) <- withManager $ \manager -> do
--                                 Response status _ bsrc <- (httpLbs req manager) `L.catch` errHandler
--                                 return (status, bsrc)
      putStrLn $ "Got status: " ++ (show status)
      case status of
        (Status 200 _) -> do
                    case (decodeAccessToken content) of
                      Nothing -> do
                            putStrLn "parse error for access token"
                            return Nothing
                      Just t -> return $ Just (token t)

--                     let r = AP.eitherResult $ AP.parse json content
--                     case r of
--                       Left msg -> do
--                              putStrLn $ "Json error" ++ msg
--                              return Nothing
--                       Right d -> do
--                              putStrLn "json ok"
--                              let Object obj = d
--                                  t = do
--                                    String txt <- M.lookup "access_token" obj
--                                    return txt
--                              return t 
        _ -> do
          putStrLn "invalid status"
          return Nothing

    dispatch "GET" ["forward"] = do
        render <- getUrlRender
        tm <- getRouteToMaster
        render <- getUrlRender
        toMaster <- getRouteToMaster
        let u = reqUrl `append` (render . toMaster $ PluginR "GooglePlus" [])
        liftIO . putStrLn . unpack $ u
--        tok <- liftIO $ getTemporaryCredential oauth'
        redirect $ u

    dispatch "GET" [] = do
        (state, code) <- runInputGet $ (,)
            <$> ireq textField "state"
            <*> ireq textField "code"
        liftIO $ putStrLn $ unpack $ "code : " `append` code
        render <- getUrlRender
        toMaster <- getRouteToMaster
        master <- getYesod
        let callback = render . toMaster $ PluginR "GooglePlus" []
            httpMgr = (authHttpManager master)
        macc <- liftIO $ (getAccessToken httpMgr callback) $ unpack code
        case macc of
          Just accessToken -> do
                          liftIO $ putStrLn $ unpack $ "Got accessToken! " `append` accessToken
                          minfo <- liftIO $ getUserInfo httpMgr accessToken
                          case minfo of
                            Nothing -> liftIO $ putStrLn "Failed to get userInfo"
                            Just js -> do
                                       case (decodeUserInfo js) of
                                         Nothing -> liftIO $ putStrLn "failed parse"
                                         Just i -> do
                                           liftIO $ putStrLn (show i)
                                           let creds = Creds name (userInfoId i)
                                                       (userInfoToCredsExtra i)
                                           setCreds True creds
          Nothing -> do
                    liftIO $ putStrLn $ "failed to get access_token"
                    return ()
        -- Check the 
        -- otherwise someone can call this url directory
--        accTok <- liftIO $ getAccessToken oauth reqTok
--        let crId = decodeUtf8With lenientDecode $ fromJust $ lookup (pack ident) $ unCredential accTok

    dispatch _ _ = notFound
    login :: forall s m. (Route Auth -> Route m) -> GWidget s m ()
    login tm = do
        render <- lift getUrlRender
        let oaUrl = render $ tm $ oauthUrl name
        addHtml $
          [shamlet|
<div ."googlePlusOAuth">
  <a ."googlePlusOAuthLogin" href=#{oaUrl}>Logn with #{name}
|]


authGooglePlus :: YesodAuth m =>
               String -- ^ Consumer Key
            -> String -- ^ Consumer Secret
            -> AuthPlugin m
authGooglePlus = authGoogleOAuth "GooglePlus"
                 "google_id"
                 "https://accounts.google.com/o/oauth2/auth?response_type=code&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile&state=%2Fprofile&client_id=783306877203.apps.googleusercontent.com&redirect_uri=" -- http://localhost:3000/auth/page/GooglePlus" -- Request URL
              "https://accounts.google.com/o/oauth2/token" -- Access Token URL
              "https://accounts.google.com/o/oauth2/auth" -- Authorize URL


              

googlePlusUrl :: AuthRoute
googlePlusUrl = oauthUrl "GooglePlus"

--bsToText :: LBS.ByteString -> Text
--bsToText = decodeUtf8With lenientDecode
