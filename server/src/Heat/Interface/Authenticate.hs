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
                                    Token(..)) where

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
data Token = Token
             { userid :: HexString   -- ^Unique user identity
             , token :: Text         -- ^The JSON Web token
             } deriving (Generic, Show)

instance ToJSON Token
