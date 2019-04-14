{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lib
    ( someFunc
    ) where

import           Data.Text (Text)
import           Yesod

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

data App = App

mkYesod "App" [parseRoutes|
/authenticate AuthenticateR GET
/api ApiR POST
|]

instance Yesod App

postApiR :: Handler ()
postApiR = notFound

getAuthenticateR :: Handler TypedContent
getAuthenticateR = selectRep $ do
    provideJson person
  where
    person@Person {..} = Person "Michael" 28

someFunc :: IO ()
someFunc = warp 3000 App
