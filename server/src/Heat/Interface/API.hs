{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Heat.Interface.API
-- Description : The interface description of the API endpoint
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the interfaces for the API endpoint
module Heat.Interface.API (API(..)) where

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
import Heat.Model

-- |The API information
data API = API {
  name:: Text
  , version:: Text
  } deriving (Generic, Show)

instance ToJSON API
