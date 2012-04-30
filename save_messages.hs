{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables #-}

-- Simple JSON usage for Google Plus API with Text.JSON
-- Target json is available at 
--   wget -O gochisou.json --no-check-certificate "https://www.googleapis.com/plus/v1/activities?query=%23%E3%81%94%E3%81%A1%E3%81%9D%E3%81%86%E3%83%95%E3%82%A9%E3%83%88&language=ja&maxResults=20&orderBy=recent&pp=1&key={API Key}"
--
--
-- API reference for this "search" endpoint is available at
--   https://developers.google.com/+/api/latest/activities/search
--
-- Usage:
--   runhaskell hoge.hs gochisou.json
import Text.JSON
import System
import System.IO

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Applicative
import Data.Time.Clock
import Data.Text (unpack, pack, append)
import Yesod
import Foundation
import Handler.Follow
import Database.Persist.Store

import Model

-- Gets Images from the following structure
-- It returns empty list Images if the entry doesn't have any images
-- {
--   "object" : {
--     "attachments" : {
--       "image" : "http://...."
--       "type"  : "image/jpeg"
--     ...
-- }
imgsFromJSObject :: JSObject JSValue -> Result Images
imgsFromJSObject obj = do
  case (imgsFromJSObject_ obj) of
    Error e -> return $ Images []
    ok -> ok

-- Helper for imgsFromJSObject_
imgsFromJSObject_ :: JSObject JSValue -> Result Images
imgsFromJSObject_ obj = do
  JSObject objects <- valFromObj "object" obj :: Result JSValue
  attachments <- valFromObj "attachments" objects :: Result JSValue
  let JSArray atts = attachments
  urls <- mapM getImg atts
  return $ Images urls
      where getImg :: JSValue -> Result Image
            getImg jsv = do
                         let JSObject obj = jsv
                         a <- valFromObj "image" obj
                         jss <- valFromObj "url" a
                         filetype :: String <- valFromObj "type" a
                         let imgtype = case filetype of
                                         "image/jpeg" -> JPG
                                         "image/png"  -> PNG
                         return $ Image (fromJSString jss) imgtype

-- Get User from actor
-- "actor" : {
--   "displayName" : "..."
--   "image" : {
--     "url" : "http:// ... ",
--   ... (This doesn't have filetype somehow)
userFromActor :: JSValue -> Result GUser
userFromActor jsv = do
  let JSObject obj = jsv
  displayName <- valFromObj "displayName" obj
  googleId <- valFromObj "id" obj
  image <- valFromObj "image" obj
  iconURL <- valFromObj "url" image
  profileURL <- valFromObj "url" obj
  return $ (GUser (fromJSString googleId)
            (fromJSString displayName)
            (fromJSString iconURL)
            (fromJSString profileURL))

msgFromJSObject :: JSObject JSValue -> Result OsojiMessage
msgFromJSObject obj = do
  title <- valFromObj "title" obj
  actor <- valFromObj "actor" obj
  linkURL <- valFromObj "url" obj
  verb <- valFromObj "verb" obj
  user <- userFromActor actor
  imgs <- imgsFromJSObject obj
  return $ GMsg user (fromJSString verb) (Message $ fromJSString title) imgs linkURL

jsonToGMsgs :: String -> Result [OsojiMessage]
jsonToGMsgs s = do
  jsonRoot <- decode s
  items_ <- valFromObj "items" jsonRoot :: Result JSValue
  let JSArray items = items_
  mapM iter items
       where iter :: JSValue -> Result OsojiMessage
             iter jsv = do
                  let JSObject c = jsv
                  msgFromJSObject c

readOsojiMessages :: String -> Either String [OsojiMessage]
readOsojiMessages fileContent = do
  resultToEither $ jsonToGMsgs $ fileContent

printOsojiMessages :: [OsojiMessage] -> IO ()
printOsojiMessages msgs = do
  mapM_ (\x -> putStrLn $ show x) msgs

printMessages :: String -> IO ()
printMessages filepath = do
  h <- openFile filepath ReadMode
  fileContent <- hGetContents h
  case (readOsojiMessages fileContent) of
    Left e -> putStrLn e
    Right msgs -> printOsojiMessages msgs


dbFileName = "OsojiPhoto_production.sqlite3"

saveOsojiMessage :: OsojiMessage -> SqlPersist IO (Key SqlPersist (OsojiPostGeneric SqlPersist))
saveOsojiMessage (GMsg (GUser googleId name userImageURL userLink) verb
                           (Message message) (Images imgs) linkURL) = do
  mGUser <- selectFirst [ OsojiUserGoogleId ==. googleId ] [LimitTo 1]
  (userId, user) <- case mGUser of
              Just (Entity k u) -> return $ (k, u)-- mGUser :: Maybe (Key b val, val)
              Nothing -> do
                let u = OsojiUser googleId name userImageURL userLink
                k <- insert $ u
                return (k, u)
  postId <- insert $ OsojiPost message linkURL userId
  mapM (\(Image url t) -> 
        insert $ OsojiImage url "aaa" postId) imgs
  _ <- saveNewsOfPost (userId, user) postId
  return postId

saveNewsOfPost :: (OsojiUserId, OsojiUser) -> OsojiPostId -> SqlPersist IO [NewsId]
saveNewsOfPost (ouserId, ouser) postId = do
    currentTime <- liftIO $ getCurrentTime
    follows <- selectList [ FollowToUserId ==. ouserId ] [ Desc FollowId ]
    followingUsers <- selectList [ UserId <-. ((followFromUserId.entityVal)
                                               <$> follows) ] []
    let (Key (PersistInt64 pid)) = postId
        linkURL = "/message/" `append` (pack.show) pid
        newsMessage = "New post from " `append` (pack.osojiUserName) ouser
        insertNews userId = insert $ News userId "NewPost" linkURL newsMessage
                            currentTime Nothing
    mapM (insertNews . entityKey) followingUsers


saveOsojiMessageIfNew :: OsojiMessage -> SqlPersist IO (Key SqlPersist (OsojiPostGeneric SqlPersist))
saveOsojiMessageIfNew omsg@(GMsg _ _ _ _ linkURL) = do
  mResult <- selectFirst [ OsojiPostLinkURL ==. linkURL ] [LimitTo 1]
  case mResult of
    Just (Entity key _) -> do
      liftIO $ putStrLn $ "Already in DB: " ++ linkURL
      return key
    Nothing -> do
      liftIO $ putStrLn $ "Saved one message: " ++ linkURL
      saveOsojiMessage omsg

saveOsojiMessages :: [OsojiMessage] -> IO [(Key SqlPersist (OsojiPost))]
--saveOsojiMessages msgs = putStrLn "hello"
saveOsojiMessages msgs = withSqliteConn dbFileName $ runSqlConn $ do
                           sequence $ map saveOsojiMessageIfNew msgs
--                           createOsojiMessage $ msgs !! 0
  

saveMessagesOnFileName :: String -> IO ()
saveMessagesOnFileName filepath = do
  h <- openFile filepath ReadMode
  fileContent <- hGetContents h
  case (readOsojiMessages fileContent) of
    Left e -> putStrLn e
    Right msgs -> void $ saveOsojiMessages (filter isPost msgs)
                  where isPost (GMsg _ "post" _ _ _) = True
                        isPost _ = False

syncdb :: IO ()
syncdb = withSqliteConn dbFileName $ runSqlConn $ do
           runMigration migrateAll

main :: IO ()
main = do
  [jsonFileName] <- getArgs
--  printMessages jsonFileName -- "gochisou.json"
  saveMessagesOnFileName jsonFileName
  putStrLn "Finished"
