{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Heat.Data.Role
-- Description : The definition of a users role
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the role data type and its helper functions
module Heat.Data.Role where

--
-- External imports
--
import GHC.Generics
import Database.Persist.TH
import Data.Char (toLower)
import Data.Aeson (ToJSON(..), FromJSON(..), object, pairs, (.:), Options(..), genericParseJSON,
                   defaultOptions, genericToJSON, genericToEncoding)

-- |Role of a user
data UserRole = Simple
              | Administrator
              | Device
              deriving (Generic, Show, Read, Eq)

customOptions = defaultOptions
                { constructorTagModifier = map toLower
                }

instance ToJSON UserRole where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions
    
instance FromJSON UserRole where
    parseJSON = genericParseJSON customOptions
    
-- instance ToJSON UserRole
-- instance FromJSON UserRole
derivePersistField "UserRole"

