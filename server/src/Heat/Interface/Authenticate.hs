{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Heat.Interface.Authenticate
-- Description : The interface description for the login functionality
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the interfaces for the user handler API
module Heat.Interface.Authenticate (Authenticate(..),
                                    UserInfo(..)) where

--
-- External imports
--
import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.TH
import Data.HexString

--
-- Heat imports
--
import Heat.Data.Conversions
import Heat.Data.Role (UserRole(..))
import Heat.Model

-- |Authenticate body description, comes with the POST
data Authenticate = Authenticate
  { username :: Text  -- ^The username of the user
  , password  :: Text -- ^The password to authenticate the user with
  } deriving (Generic, Show)

instance FromJSON Authenticate

-- |The JSON Web token returned after authentication, response to the POST
data UserInfo = UserInfo
             { iuserid :: HexString   -- ^Unique user identity
             , itoken :: Text         -- ^The JSON Web token
             , iusername :: Text      -- ^The username
             , irole :: UserRole       -- ^The role of the user
             , ilevel :: Int          -- ^The level of the user           
             , iemail :: Text         -- ^Email address to the user
             } deriving (Generic, Show)

$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 1 -- Get rid of the first character in the field names
     } ''UserInfo)

