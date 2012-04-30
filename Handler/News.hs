{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Handler.News where

import Import
import Database.Persist.Sqlite
import Data.Time.Clock
import Data.Text (unpack, pack)
import Foundation


data NewsType = NewPost | NewFollow
              deriving Show

getReadNewsR :: NewsId -> Handler RepHtml
getReadNewsR nid = do
  mNews <- runDB $ selectFirst [ NewsId ==. nid ] [Desc NewsId]
  case mNews of
    Nothing -> redirect ("http://google.com" :: String)
    Just (Entity k news) -> do
                     currentTime <- liftIO $ getCurrentTime
                     runDB $ update k [ NewsReadAt =. (Just currentTime) ]
                     redirect (newsLinkURL news)


getListNewsR :: Int -> Int -> Handler RepJson
getListNewsR resultsPerPage pageNumber = do
  Entity u _ <- requireAuth
  render <- getUrlRender
  knews <- runDB $ selectList [(NewsUserId ==. u)]
           [ Desc NewsCreatedAt,
             LimitTo resultsPerPage,
             OffsetBy $ (pageNumber - 1) * resultsPerPage
           ]
  jsonToRepJson $ object [ ("news", array $ newsToJson render <$> knews) ]
      where
        newsToJson render (Entity k news) =
            object [("message", (newsMessage news))
                    ,("url", render (ReadNewsR k))
                    ,("createdAt", pack . show $ (newsCreatedAt news))
                    ,("readAt", pack . show $ (newsReadAt news))
                    ,("type", (newsType news))]

putNews :: UserId -> Text -> OsojiPhotoRoute -> Text -> Handler NewsId
putNews userId newsType route message = do
  render <- getUrlRender
  currentTime <- liftIO getCurrentTime
  let linkURL = render route
      news = News userId newsType linkURL message currentTime Nothing
  runDB $ insert news

