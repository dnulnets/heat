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

module Heat.Authenticate (jsonToToken,
                          tokenToJson,
                          postAuthenticateR) where

import GHC.Generics

import Data.Text (Text)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message as AuthMsg
import Network.HTTP.Types ( status400, status401 )
import Network.Wai (requestHeaders)
import Data.Map as Map (fromList, (!?))
import Data.Aeson (fromJSON, Result(..))

import Heat.Settings
import Heat.Foundation
import Heat.Utils.JWT
import Heat.Data.UserInfo

--
-- Authenticate object for the authenticate api
--
data Authenticate = Authenticate
  { username :: Text,
    password  :: Text
  } deriving (Generic, Show)

instance ToJSON Authenticate
instance FromJSON Authenticate

data Token = Token
  { token :: Text
  } deriving (Generic, Show)

instance ToJSON Token
instance FromJSON Token

-- | Authenticate the user and create a JSON Web Token that is returned so it can be used
-- | for following calls
postAuthenticateR :: Handler TypedContent
postAuthenticateR = do
  foo <- requireCheckJsonBody :: Handler Authenticate
  secret <- jwtSecret . settings <$> getYesod
  token <- return $ jsonToToken secret $ toJSON $ UserInfo "67565-63258"
  selectRep $ do
    provideJson $ Token token
