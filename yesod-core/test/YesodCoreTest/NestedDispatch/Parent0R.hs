{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module YesodCoreTest.NestedDispatch.Parent0R where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core
import YesodCoreTest.NestedDispatch.Parent0R.Child0R

mkYesodOpts (setUrlToDispatchSiteHook 'installDispatchMarker $ setFocusOnNestedRoute (Just "Parent0R") defaultOpts) "App" nestedDispatchResources

instance DispatchMarker (WithParentArgs Parent0R) where
    dispatchMarker (WithParentArgs i Parent0IndexR) = "Parent0IndexR:" <> Text.pack (show i)
    dispatchMarker (WithParentArgs i Parent0MarkerR) = "Parent0MarkerR:" <> Text.pack (show i)
    dispatchMarker (WithParentArgs i (Child0R _ _)) = "Child0R:" <> Text.pack (show i)

instance DispatchMarker (WithParentArgs Child0R) where
    dispatchMarker (WithParentArgs (i, t) _) =
        "Child0R:" <> Text.pack (show i) <> ":" <> t

getParent0IndexR :: Int -> HandlerFor App Text
getParent0IndexR = pure . Text.pack . show

getParent0MarkerR :: Int -> HandlerFor App Text
getParent0MarkerR _ = do
    app <- getYesod
    pure $ fromMaybe "missing marker" $ appDispatchMarker app
