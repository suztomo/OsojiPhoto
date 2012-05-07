{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Handler.Shared where

import Import
import Database.Persist.Store
import Data.Text (unpack, pack)

data FollowForm = FollowForm {
      targetGoogleId :: Text
      } deriving Show

followButton :: Text -> Html -> MForm OsojiPhoto OsojiPhoto (FormResult FollowForm, Widget)
followButton targetId extra = do
  (textRes, targetIdInput) <- mreq hiddenField "" (Just targetId)
  let rs = FollowForm <$> textRes
  let w = do 
        $(widgetFile "follow-button")
  return (rs, w)

