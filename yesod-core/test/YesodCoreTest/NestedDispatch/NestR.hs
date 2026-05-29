{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.NestedDispatch.NestR where

import Yesod.Core
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources

mkYesodOpts (setUrlToDispatchSiteHook 'installDispatchMarker $ setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

instance DispatchMarker NestR where
    dispatchMarker NestIndexR = "NestIndexR"
    dispatchMarker NestMarkerR = "NestMarkerR"
    dispatchMarker (MarkerSubsiteR _) = "MarkerSubsiteR"

instance YesodSubDispatch MarkerSubsite App where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesMarkerSubsite)

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

getNestMarkerR :: HandlerFor App Text
getNestMarkerR = do
    app <- getYesod
    pure $ fromMaybe "missing marker" $ appDispatchMarker app

postNestIndexR :: HandlerFor App String
postNestIndexR = pure "hello"

getMarkerSubsiteMarkerR :: SubHandlerFor MarkerSubsite App Text
getMarkerSubsiteMarkerR = do
    app <- getYesod
    pure $ fromMaybe "missing marker" $ appDispatchMarker app
