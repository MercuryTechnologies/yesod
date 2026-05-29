{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.NestR where

import Yesod.Core
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import YesodCoreTest.NestedDispatch.Resources

mkYesodOpts (setUrlToDispatchSiteHook 'installDispatchMarker $ setFocusOnNestedRoute (Just "NestR") defaultOpts) "App" nestedDispatchResources

instance DispatchMarker NestR where
    dispatchMarker NestIndexR = "NestIndexR"
    dispatchMarker NestMarkerR = "NestMarkerR"

getNestIndexR :: HandlerFor App Text
getNestIndexR = pure "getNestIndexR"

getNestMarkerR :: HandlerFor App Text
getNestMarkerR = do
    app <- getYesod
    pure $ fromMaybe "missing marker" $ appDispatchMarker app

postNestIndexR :: HandlerFor App String
postNestIndexR = pure "hello"
