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
import           Network.HTTP.Types ( status400 )

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

instance Yesod App where
    makeSessionBackend _ = return Nothing
    
postApiR :: Handler TypedContent
postApiR = do
  bdy <- waiRequest
  liftIO $ print $ show $ bdy
  addHeader "myheader" "headerdata"
  selectRep $ do
    provideJson $ Person "Michael" 28
  sendResponseStatus status400 ("DELETED" :: Text)

getAuthenticateR :: Handler TypedContent
getAuthenticateR = selectRep $ do
  provideJson person
    where
      person@Person {..} = Person "Michael" 28
      
someFunc :: IO ()
someFunc = warp 3000 App

    -- body <- runRequestBody
    -- liftIO $ print $ show $ fst body
    -- bdy <- _ lift $ waiRequest
    -- $ show $ bdy
