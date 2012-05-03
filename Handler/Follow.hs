{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Handler.Follow where

import Import
import Database.Persist.Sqlite
import Data.Text (unpack)
import Handler.Root hiding (userToJson)

followee :: (Entity Follow) -> Key SqlPersist OsojiUser
followee (Entity _ f) = followToUserId f
following :: (Entity Follow) -> Key SqlPersist User
following (Entity _ f) = followFromUserId f

getFollowingMessagesR :: Handler RepJson
getFollowingMessagesR = do
  mAuth <- maybeAuth 
  case mAuth of
    Nothing -> errorMsgToRepJson "Not authorized yet"
    Just (Entity k _) -> do
                          follows <- runDB $ selectList [ FollowFromUserId ==. k ] [ Desc FollowId ]
                          posts <- runDB $ selectList [ OsojiPostUserId <-. (followee <$> follows) ] [ ]
                          postHandler posts
                                             

osojiUserToJson (Entity _ u) = object [("googleId", (osojiUserGoogleId u)),
                                  ("name" , (osojiUserName u)),
                                  ("imageURL", (osojiUserImageURL u))
                                 ]

userToJson (Entity _ u) = object [("email", (userEmail u))
                                 ,("ident", (userIdent u))]
                                  

getFollowingUsersR :: Handler RepJson
getFollowingUsersR = do
  mAuth <- maybeAuth 
  case mAuth of
    Nothing -> errorMsgToRepJson "Not authorized yet"
    Just (Entity k _) -> do
               follows <- runDB $ selectList [ FollowFromUserId ==. k ] [ Desc FollowId ]
               followedUsers <- runDB $ selectList [ OsojiUserId <-. (followee <$> follows) ] [ Desc OsojiUserGoogleId ]
               jsonToRepJson $ array $ map osojiUserToJson followedUsers


-- Get followers by user's id. This ID doesn't any relationship with Google ID
getFollowersR :: OsojiUserId -> Handler RepJson
getFollowersR userId = do
  follows <- runDB $ selectList [ FollowToUserId ==. userId ] []
  followingUsers <- runDB $ selectList [ UserId <-. (following <$> follows) ] []
  let users = array $ map userToJson followingUsers
  jsonToRepJson $ object [("users", users)]

getIsFollowingR :: String -> Handler RepJson
getIsFollowingR googleId = do
  (Entity k _) <- requireAuth  
  res <- runDB $ selectFirst [OsojiUserGoogleId ==. googleId] []
  case res of
    Nothing -> notFound
    Just (Entity targetKey _) -> do
             r <- runDB $ selectFirst [ FollowFromUserId ==. k,
                                        FollowToUserId   ==. targetKey ] []
             jsonToRepJson $ object [("following",
                                      maybe "false" (const "true" :: a-> Text) r)]


getFollowingPostsR :: Handler RepJson
getFollowingPostsR = do
  mAuth <- maybeAuth
  case mAuth of
    Nothing -> errorMsgToRepJson "Not authorized yet"
    Just (Entity k _) -> do
               follows <- runDB $ selectList [ FollowFromUserId ==. k ] [ Desc FollowId ]
               posts <- runDB $ selectList [ OsojiPostUserId <-. (followee <$> follows) ] []
               postHandler posts


postAddFollowR :: Handler RepJson
postAddFollowR = do
  (Entity k _) <- requireAuth
  ((result, _), _) <- runFormPost (followButton "dummy")
  case result of
    FormSuccess (FollowForm targetId) -> do
                   res <- runDB $ selectFirst
                          [OsojiUserGoogleId ==. (unpack targetId)] []
                   case res of
                     Nothing -> notFound
                     Just (Entity targetKey _) -> do
                              _ <- runDB $ insert $ Follow k targetKey
                              jsonToRepJson $ object
                                                [("result", "follow ok" :: Text)]
    FormFailure err -> errorMsgToRepJson $ show err

postRemoveFollowR :: Handler RepJson
postRemoveFollowR = do
  (Entity k _) <- requireAuth
  ((result, _), _) <- runFormPost (followButton "dummy")
  case result of
    FormSuccess (FollowForm targetId) -> do
                      res <- runDB $ selectFirst
                             [OsojiUserGoogleId ==. (unpack targetId)] []
                      case res of
                        Nothing -> notFound
                        Just (Entity targetKey _) -> do
                                           _ <- runDB $ deleteWhere [(FollowFromUserId ==. k),
                                                                     (FollowToUserId ==. targetKey)]
                                           jsonToRepJson $ object [
                                                              ("result",
                                                               "remove ok" :: Text)]
    FormFailure err -> errorMsgToRepJson $ show err

--  
--      where targetKey = 
