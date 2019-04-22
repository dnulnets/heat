{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Heat.Data.UserInfo
-- Description : The user information stored in a JSON Web Token
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the user information that is stored in a JSON Web Token
module Heat.Data.UserInfo (UserInfo(..)) where

import Data.Int (Int64)

--
-- External imports
--
import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

-- |User information that is used in the JSON Web Token
data UserInfo = UserInfo {
  userid :: Int64 -- ^A user identity
  } deriving (Generic, Show)

instance ToJSON UserInfo
instance FromJSON UserInfo
