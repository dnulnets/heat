{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Heat.Handler.UserAuthenticate
-- Description : The user handlers
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the handlers for the user object
module Heat.Handler.User (putUserR, getUserR) where

--
-- External imports
--
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import Data.Time.Clock.System (getSystemTime,
                               SystemTime(..))
import Network.HTTP.Types.Status (created201)
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
import Heat.Data.Role (UserRole(..))
import Heat.Utils.Password (authHashPassword, authValidatePassword)

-- |NewUser body description, comes with the PUT
data NewUser = NewUser
  { username :: Text  -- ^The username of the user
  , password  :: Text -- ^The password to authenticate the user with
  , role :: UserRole  -- ^The role of the user
  , level :: Int      -- ^The level of the user within its role
  , email :: Text     -- ^The email address to the user
  } deriving (Generic, Show)

instance FromJSON NewUser
instance ToJSON NewUser

data NewUserKey = NewUserKey
  { userid :: Key User
  } deriving (Generic, Show)

instance ToJSON NewUserKey

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
putUserR :: Handler ()
putUserR = do
  newUser <- requireCheckJsonBody :: Handler NewUser
  appset <- appSettings <$> getYesod
  hpwd <- liftIO $ authHashPassword (passwordCost appset) (password newUser)
  key <- runDB $ insert400 $ User (username newUser) (decodeUtf8 hpwd) (role newUser) (level newUser) (email newUser)
  sendResponseStatus created201 $ toJSON $ NewUserKey key

getUserR :: Handler Value
getUserR = do
  returnJson $ toJSON $ NewUser "tomas" "password" Simple 1 "tomas@stenlund.cc"
  
