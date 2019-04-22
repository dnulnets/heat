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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock.System (getSystemTime,
                               SystemTime(..))
import Crypto.KDF.BCrypt (hashPassword,validatePassword)

--
-- Internal imports
--
import Yesod
import Heat.Model
--
-- Heat imports
--
import Heat.Settings (AppSettings(..))
import Heat.Foundation (appSettings, Handler)
import Heat.Utils.JWT (jsonToToken)
import Heat.Data.UserInfo (UserInfo (..))

-- |Authenticate body description, comes with the POST
data Authenticate = Authenticate
  { username :: Text  -- ^The username of the user
  , password  :: Text -- ^The password to authenticate the user with
  } deriving (Generic, Show)

instance FromJSON Authenticate

-- |The JSON Web token returned after authentication, response to the POST
data Token = Token
  { token :: Text -- ^The JSON Web token
  } deriving (Generic, Show)

instance ToJSON Token

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
postAuthenticateR :: Handler Value
postAuthenticateR = do
  auth <- requireCheckJsonBody :: Handler Authenticate
  hsh <- liftIO $ hash "mandelmassa"
  liftIO $ print $ hsh
  seconds <- liftIO $ systemSeconds <$> getSystemTime
  secret <- tokenSecret . appSettings <$> getYesod
  length <- tokenExpiration . appSettings <$> getYesod
  dbuser <- runDB $ getBy $ UniqueUserUsername $ username auth
  case dbuser of
    Just (Entity userId user) | validPassword (userPassword user) (password auth) -> do
      token <- return $ jsonToToken secret (fromIntegral seconds) length $ toJSON userId
      returnJson $ Token token
    _ -> notAuthenticated


validPassword::Text->Text->Bool
validPassword uid pwd = validatePassword (encodeUtf8 pwd) (B.pack "$2b$12$4G0n1i213IFmAs9IzKb6MOfvIDMu39f.MRX9PeZr4nJ48ccPSTBUq") 

hash :: Text->IO ByteString
hash pwd = do
  result <- hashPassword 12 (encodeUtf8 pwd) :: IO B.ByteString
  return result
  
