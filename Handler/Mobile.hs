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

mobileLayout :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
mobileLayout widget = do
        mmsg <- getMessage
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <- widgetToPageContent $ do
            $(widgetFile "mobile-default")
        hamletToRepHtml $(hamletFile "templates/mobile-layout-wrapper.hamlet")

-- And we'll spell out the handler type signature.
getMobileRootR :: Handler RepHtml
getMobileRootR = do
  mobileLayout $ do
    setTitle "Hello Mobile"
    $(widgetFile "mobile-home")    

getMobileAboutR :: Handler RepHtml
getMobileAboutR =
  mobileLayout $ do
    setTitle "おそうじフォト"
    $(widgetFile "mobile-about")
