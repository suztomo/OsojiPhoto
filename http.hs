{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

import Prelude (map, ($), String, Bool(..), lookup, putStrLn, IO, (++), show, (.))
import Yesod.Auth
import Yesod.Form
import Yesod.Handler
import Yesod.Widget
import Data.Either
import Text.Hamlet (shamlet)
import Web.Authenticate.OAuth
import Data.Maybe
import Data.String
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Arrow ((***))
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Text (Text, unpack, append)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Applicative ((<$>), (<*>))
import Network.HTTP.Conduit
import Control.Exception as X
import Control.Exception.Lifted as L
import Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString.Lazy as AP (Result(..),parse,eitherResult)
import Network.HTTP.Types
--import Data.Aeson
--import Data.Aeson.Types
import Data.Maybe (catMaybes)
import qualified Data.Map as M

oauthUrl :: Text -> AuthRoute
oauthUrl name = PluginR name ["forward"]

postData :: String -> [(BS.ByteString, BS.ByteString)] -> IO (Maybe LBS.ByteString)
postData url payload = do
    parsedUrl <- parseUrl url
    let req = urlEncodedBody payload $ parsedUrl
        errHandler :: ResourceIO m => HttpException
                   -> ResourceT m (Response LBS.ByteString)
        errHandler (StatusCodeException status hdrs) = return $ Response status hdrs LBS.empty
    (status, content) <- withManager $ \manager -> do
                           Response status _ bsrc <- (httpLbs req manager) `L.catch` errHandler
                           return (status, bsrc)
    putStrLn $ "Got status: " ++ (show status)
    case status of
      (Status 200 _) -> return $ Just content
      _ -> do
        putStrLn "invalid status"
        return Nothing

main :: IO ()
main = do
  d <- postData "http://localhost:8000/" [("aaa", "bbb")]
  case d of
    Nothing -> return ()
    Just c -> LBS.putStrLn $ c
