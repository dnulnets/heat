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
import Database.Persist.Postgresql (PostgresConf (..))

-- |Our application settings
data AppSettings = AppSettings {
  
  tokenSecret :: Text            -- ^The secret used to sign and verify a JSON Web Token
  , tokenExpiration :: Integer   -- ^The expiration time of the token in seconds
  
  , databaseConf :: PostgresConf -- ^The database configuration

  , passwordCost:: Integer       -- ^The cost for the bcrypt password hash generation
  
  }

-- |A default setting for our application
defaultSettings = AppSettings {
  
  tokenSecret = "mandelmassa"     -- ^The default token secret
  ,tokenExpiration = 60*60         -- ^The default token is only valid for 1 hour
  
  ,databaseConf = PostgresConf "postgresql://heatserver:heatserver@localhost/heat" 5 -- ^The default postgres database
  
  ,passwordCost = 10 -- ^The default password hashing cost
  }
