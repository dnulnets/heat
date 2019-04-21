{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Heat.Settings
-- Description : The application settings
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the possible settings for the application and also
-- a default setting.
module Heat.Settings (AppSettings(..),
                      defaultSettings) where

--
-- External imports
--
import Data.Text (Text)

-- |Our application settings
data AppSettings = AppSettings {
  jwtSecret :: Text -- ^The secret used to sign and verify a JSON Web Token
  }

-- |A default setting for our application
defaultSettings = AppSettings {
  jwtSecret = "mandelmassa"
  }
