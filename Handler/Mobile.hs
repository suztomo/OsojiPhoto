{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances,
    RankNTypes, CPP #-}

module Handler.Mobile where
import Import
import Prelude
import Yesod
import Settings (widgetFile)
import Text.Hamlet (hamletFile)
import Handler.Shared

-- import Foundation (mobileLayout)
mobileLayout :: forall sub a. GWidget sub OsojiPhoto a -> GHandler sub OsojiPhoto RepHtml
mobileLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            $(widgetFile "mobile-default")
        hamletToRepHtml $(hamletFile "templates/mobile-layout-wrapper.hamlet")

-- And we'll spell out the handler type signature.
getMobileRootR :: Handler RepHtml
getMobileRootR = do
  mobileLayout $ do
    setTitle "おそうじフォト"
    $(widgetFile "mobile-home")    

getMobileAboutR :: Handler RepHtml
getMobileAboutR =
  mobileLayout $ do
    setTitle "このサイトについて"
    $(widgetFile "mobile-about")

getMobilePublicR :: Handler RepHtml
getMobilePublicR = mobileLayout $ do
  setTitle "最新フォト"
  $(widgetFile "mobile-public-photos")

getMobileUserR :: Text -> Handler RepHtml
getMobileUserR target = do
  ((_, followWidget), _) <- generateFormPost (followButton target)
  mobileLayout $ do
    setTitle "ユーザ"
    $(widgetFile "mobile-user")
