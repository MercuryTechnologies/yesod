{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.Resources where

import Data.Text (Text)
import Yesod.Core
import Yesod.Routes.TH.Types

data App = App
    { appDispatchMarker :: Maybe Text
    }

data MarkerSubsite = MarkerSubsite

defaultApp :: App
defaultApp = App
    { appDispatchMarker = Nothing
    }

getMarkerSubsite :: App -> MarkerSubsite
getMarkerSubsite _ = MarkerSubsite

mkYesodSubData "MarkerSubsite" [parseRoutes|
/marker MarkerSubsiteMarkerR GET
|]

class DispatchMarker url where
    dispatchMarker :: url -> Text

installDispatchMarker :: DispatchMarker url => url -> App -> App
installDispatchMarker url app =
    app
        { appDispatchMarker = Just (dispatchMarker url)
        }

nestedDispatchResources :: [ResourceTree String]
nestedDispatchResources = [parseRoutes|
/     HomeR GET !home
/json JsonR GET
/nest NestR:
    / NestIndexR GET POST
    /marker NestMarkerR GET
    /sub MarkerSubsiteR MarkerSubsite getMarkerSubsite

/parent/#Int ParentR:
    /#Text/child1 Child1R !child
    /#Int/child2 Child2R !child GET POST

/parent0/#Int Parent0R:
    / Parent0IndexR GET
    /marker Parent0MarkerR GET

    /child0/#Text Child0R:
        / ParentChildIndexR GET POST
        /#String ParentChildR GET POST

/nest OtherNestR:
    /foo NestFooR GET

/blah BlahR:
    /   BlahIndexR GET

/robots.txt RobotsR:
    /   RobotsIndexR
|]
