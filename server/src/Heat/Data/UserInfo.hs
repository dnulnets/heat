{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Heat.Data.UserInfo (UserInfo(..)) where

import GHC.Generics

import Data.Text (Text)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message as AuthMsg
import Network.HTTP.Types ( status400, status401 )
import Network.Wai (requestHeaders)
import Data.Map as Map (fromList, (!?))
import Data.Aeson (fromJSON, Result(..))

--
-- The user info object that is used by the JSON Web Token
--
data UserInfo = UserInfo
  {
    userid :: Text
  } deriving (Generic, Show)

instance ToJSON UserInfo
instance FromJSON UserInfo
