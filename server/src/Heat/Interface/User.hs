{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Heat.Interface.User
-- Description : The interface description of the user handler API
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the interfaces for the user handler API
module Heat.Interface.User (CreateUser(..),
                            RetrieveUser(..),
                            UpdateUser (..),
                            UserIdentity(..)) where

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

-- |NewUser body description, comes with the PUT
data CreateUser = CreateUser
  { cusername :: Text  -- ^The username of the user
  , cpassword  :: Text -- ^The password to authenticate the user with
  , crole :: UserRole  -- ^The role of the user
  , clevel :: Int      -- ^The level of the user within its role
  , cemail :: Text     -- ^The email address to the user
  } deriving (Generic, Show)

$(deriveJSON defaultOptions {
    fieldLabelModifier = drop 1 -- Get rid of the first character in the field names
  } ''CreateUser)

-- |User body description, comes with the CRUD operations
data RetrieveUser = RetrieveUser
  { ruserid :: HexString     -- ^The unique user id of the user
  , rusername :: Text        -- ^The username of the user
  , rrole :: UserRole        -- ^The role of the user
  , rlevel :: Int            -- ^The level of the user within its role
  , remail :: Text           -- ^The email address to the user
  } deriving (Generic, Show)

$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 1 -- Get rid of the first character in the field names
     } ''RetrieveUser)

-- |User body description, comes with the CRUD operations
data UpdateUser = UpdateUser
  { uusername :: Maybe Text   -- ^The username of the user
  , upassword :: Maybe Text    -- ^The password for the user
  , urole :: Maybe UserRole   -- ^The role of the user
  , ulevel :: Maybe Int       -- ^The level of the user within its role
  , uemail :: Maybe Text      -- ^The email address to the user
  } deriving (Generic, Show)

$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 1 -- Get rid of the first character in the field names
     } ''UpdateUser)

-- | The user identity, used when creating and deleting users
data UserIdentity = UserIdentity
  { userid :: HexString -- ^The users unique identity, machine generated
  } deriving (Generic, Show)

instance ToJSON UserIdentity
