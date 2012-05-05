{-# LANGUAGE OverloadedStrings  #-}
{- -*- coding: utf-8 -*- -}
module AMQPMsgs where
import Network.AMQP
import Data.Aeson
import Data.Text
import Data.Maybe
import Control.Applicative
import Control.Monad
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

type Email = Text
type Name = Text
data NewPostMsg = NewPostMsg { message :: Text
   , link :: Text
   , name :: Text
   , recipients :: [(Name, Email)] -- name and email
} deriving Show

instance FromJSON NewPostMsg where
  parseJSON (Object v) = NewPostMsg <$>
     v .: "message" <*>
     v .: "link" <*>
     v .: "name" <*>
     v .: "recipients"
  parseJSON _ = mzero

instance ToJSON NewPostMsg where
  toJSON (NewPostMsg m l n rs) = object [
    "message" .= m,
    "link"    .= l,
    "name"    .= n,
    "recipients" .= rs ]

-- for testing
mainx :: IO ()
mainx = do
  let m = NewPostMsg "hello email test日本語" "http://osojiphoto.com/hoge" "すずきともひろ" [("Tomo","tomotomotomo888@gmail.com")]
      jv = toJSON m
      bs = encode jv
  putStr $ "Encoded: "
  BL.putStrLn bs
  let m2 = fromMaybe (m {message = "failure"}) (decode bs)
  putStrLn $ "Decoded: " ++ (show m2)
  putMsgToExchange m2

-- Puts a message to our exchange "osojiphoto"
-- This is inefficient because each time it is called it creates new connection to
-- the RabbitMQ service
putMsgToExchange :: ToJSON a => a -> IO ()
putMsgToExchange msg = do
  conn <- openConnection "176.34.54.33" "/" "guest" "guest"
  chan <- openChannel conn
  --declare exchange
  let ename = "osojiphoto"
  declareExchange chan (newExchange {exchangeName = ename, exchangeType = "fanout"} )
  publishMsg chan ename ""
    newMsg {msgBody = (encode msg),
            msgDeliveryMode = Just Persistent}
  closeConnection conn
  putStrLn "AMQP connection closed"
