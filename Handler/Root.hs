{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Handler.Root where

import Import
import Database.Persist.Store
import Data.Text (unpack, pack)
import Settings.StaticFiles
import Handler.Shared
import Handler.Mobile


-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    $(logDebug) "This is a debug log message"
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "おそうじフォト"
        $(widgetFile "homepage")

getAboutR :: Handler RepHtml
getAboutR = do
    defaultLayout $ do
        setTitle "おそうじフォトについて"
        $(widgetFile "about")

getPublicHomeR :: Handler RepHtml
getPublicHomeR = defaultLayout $ do
                 setTitle "おそうじフォト"
                 $(widgetFile "publichome")
--                 $(juliusFile "publichome")
--                 toWidgetHead $ $(coffeeFile "/Users/suztomo/Documents/OsojiPhoto/OsojiPhoto/templates/coffee-test.coffee")

userToJson :: (Route OsojiPhoto -> Text) -> User -> Value
userToJson render user = object $ map iter [
                   ("name", userFamilyName),
                   ("pictureURL", userPictureURL),
                   ("link", userLink),
                   ("familyName", userFamilyName),
                   ("givenName", userGivenName),
                   ("ident", userIdent),
                   ("messagesURL", (\u -> render $ UserOsojiMessagesR $ (userIdent u)))
                  ] where iter (attr, f) = (attr, f user)

errorMsgToRepJson :: String -> Handler RepJson
errorMsgToRepJson msg = jsonToRepJson $ object [("error", msg)]

getSelfProfileR :: Handler RepJson
getSelfProfileR = do
  render <- getUrlRender
  mAuth <- maybeAuth 
  case mAuth of
    Nothing -> errorMsgToRepJson "Not authorized yet"
    Just e -> jsonToRepJson $ userToJson render (entityVal e)

getUserInfoR :: Text -> Handler RepJson
getUserInfoR ident = do
  res <- runDB $ selectFirst [UserIdent ==. ident] []
  render <- getUrlRender
  case res of
    Nothing -> notFound
    Just e -> jsonToRepJson $ userToJson render (entityVal e)

getUserOsojiMessagesR :: Text -> Handler RepJson
getUserOsojiMessagesR googleId = do
  res <- runDB $ getBy (UniqueOsojiUser (unpack googleId))
  case res of
    Nothing -> notFound
    Just e -> do
             posts <- runDB $ selectList [OsojiPostUserId ==. (entityKey e) ] []
             postHandler posts

getMessageR :: OsojiPostId -> Handler RepJson
getMessageR postId = do
  posts <- runDB $ selectList [OsojiPostId ==. postId] [Desc OsojiPostId]
  postHandler posts


postHandler :: [Entity OsojiPost]
            -> Handler RepJson
postHandler posts = do
  render <- getUrlRender
  let messageList = array $ ((postToJson render) <$> posts)
  kimages <- runDB $ selectList [ OsojiImagePostId <-. (entityKey <$> posts) ]
             [Desc OsojiImageId]
  let imageList = array $ (imageToJson <$> kimages)
  users <- runDB $ selectList [ OsojiUserId <-. ((osojiPostUserId . entityVal)
                                                 <$> posts)] []
  let userList = array $ (guserToJson render <$> users)
  jsonToRepJson $ object [
                     ("messages", messageList),
                     ("images", imageList),
                     ("users", userList)
                    ]
    where
      postToJson :: (OsojiPhotoRoute -> Text)
                 -> (Entity OsojiPost) -> Value
      postToJson render (Entity key opost) =
          let Key (PersistInt64 postId) = key
              Key (PersistInt64 userId) = osojiPostUserId opost
          in
            object [
           ("id", show postId),
           ("message", osojiPostMessage opost),
           ("link", unpack . render $ (MessageR key)),
           ("googleLink", osojiPostLinkURL opost),
           ("userId", show userId)
          ]
      imageToJson :: Entity OsojiImage -> Value
      imageToJson (Entity key oimage) =
          let Key (PersistInt64 postId) = osojiImagePostId oimage
              Key (PersistInt64 imageId) = key
          in
            object [
           ("id", show imageId),
           ("imageURL", osojiImageImageURL oimage),
           ("imageType", osojiImageImgType oimage),
           ("postId", show postId )
          ]
      guserToJson :: (OsojiPhotoRoute -> Text) ->
                    Entity OsojiUser -> Value
      guserToJson render (Entity key ouser) = 
          let Key (PersistInt64 userId) = key
          in object
                 [("name", osojiUserName ouser),
                  ("googleId", osojiUserGoogleId ouser),
                  ("link", unpack . render $ UserR (pack (osojiUserGoogleId ouser))),
                  ("mlink", unpack . render $
                          MobileUserR (pack (osojiUserGoogleId ouser))),
                  ("googleLink",  osojiUserLink ouser),
                  ("id", show userId),
                  ("pictureURL", osojiUserImageURL ouser)
                 ]
  

getOsojiMessagesR :: Handler RepJson
getOsojiMessagesR = do
  kposts <- runDB $ selectList [] [Desc OsojiPostId]
  postHandler kposts
                 

getUserR :: Text -> Handler RepHtml
getUserR target = do
  ((_, followWidget), _) <- generateFormPost (followButton target)
  defaultLayout $ do
    $(widgetFile "user")


getDeleteMessageR :: OsojiPostId -> Handler RepJson
getDeleteMessageR postId = do
  mpost <- runDB $ selectFirst [OsojiPostId ==. postId, OsojiPostDeleted ==. False ]
          [Desc OsojiPostId]
  case mpost of
    Nothing -> errorMsgToRepJson "Not found"
    Just post -> do
      mAuth <- maybeAuth 
      case mAuth of
        Nothing -> errorMsgToRepJson "Not authorized yet"
        Just user -> do
                   -- userIdent  : googleId
          mouser <- runDB $ selectFirst
                    [OsojiUserId ==. (osojiPostUserId (entityVal post))] []
          case mouser of
            Nothing -> errorMsgToRepJson "Not authorized yet"
            Just ouser -> do
              let ouserGoogleId = pack $ osojiUserGoogleId (entityVal ouser)
              let loginUserGoogleId = userIdent $ entityVal user
              case ouserGoogleId == loginUserGoogleId of
                True -> jsonToRepJson $ object [("error", "OK" :: Text)] 
                False -> jsonToRepJson $ object [("error", "unauthorized" :: Text)]