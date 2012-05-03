{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies, GADTs, FlexibleContexts, FlexibleInstances,
    CPP #-}

module Handler.Mobile where
import Import
import Prelude
import Yesod
import Settings (widgetFile)
import Text.Hamlet (hamletFile)
-- import Foundation (mobileLayout)

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
  setTitle "最新のおそうじフォト"
  $(widgetFile "mobile-public-photos")