{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, CPP #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist
import Data.Time.Clock
import GooglePlusOAuth


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFile "config/models")


type UserImageURL = String
type GoogleProfileLink = String
type GoogleId = String
data GUser = GUser GoogleId String UserImageURL GoogleProfileLink deriving Show
newtype Message = Message String  deriving Show

newtype Images = Images [Image]  deriving Show

data ImgType = JPG | PNG deriving Show
data Image = Image String ImgType deriving Show
type LinkURL = String
type Verb = String
data OsojiMessage = GMsg GUser Verb Message Images LinkURL
                       deriving (Show)

userInfoToUser :: GoogleUserInfo -> User
userInfoToUser ginfo = User
                       (userInfoId ginfo)
                       (userInfoFamilyName ginfo)
                       (userInfoGivenName ginfo)
                       (userInfoEmail ginfo)
                       (userInfoLink ginfo)
                       (userInfoPictureURL ginfo)
                       (userInfoGender ginfo)
                       (userInfoLocale ginfo)

