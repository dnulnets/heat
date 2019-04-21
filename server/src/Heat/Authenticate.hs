{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Heat.Authenticate
-- Description : The authenticate route
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the authenticate route for the application
module Heat.Authenticate (postAuthenticateR) where

import GHC.Generics

--
-- External imports
--
import Data.Text (Text)
import Yesod

--
-- Heat imports
--
import Heat.Settings
import Heat.Foundation
import Heat.Utils.JWT
import Heat.Data.UserInfo

-- |Authenticate body description
data Authenticate = Authenticate
  { username :: Text  -- ^The username of the user
  , password  :: Text -- ^The password to authenticate the user with
  } deriving (Generic, Show)

instance ToJSON Authenticate
instance FromJSON Authenticate

-- |The JSON Web token returned after authentication
data Token = Token
  { token :: Text -- ^The JSON Web token
  } deriving (Generic, Show)

instance ToJSON Token
instance FromJSON Token

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
postAuthenticateR :: Handler TypedContent
postAuthenticateR = do
  foo <- requireCheckJsonBody :: Handler Authenticate
  secret <- jwtSecret . settings <$> getYesod
  token <- return $ jsonToToken secret $ toJSON $ UserInfo "67565-63258"
  selectRep $ do
    provideJson $ Token token
