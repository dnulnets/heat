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
module Heat.Handler.User (putUserR) where

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
import Heat.Utils.Password (authHashPassword, authValidatePassword)

-- |NewUser body description, comes with the PUT
data NewUser = NewUser
  { username :: Text  -- ^The username of the user
  , password  :: Text -- ^The password to authenticate the user with
  , role :: Text      -- ^The role of the user, can be either "user" or "admin"
  , level :: Int      -- ^The level of the user within its role
  , email :: Text     -- ^The email address to the user
  } deriving (Generic, Show)

instance FromJSON NewUser

data NewUserKey = NewUserKey
  { key :: Int
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
  liftIO $ print $ show key
  sendResponseStatus created201 $ toJSON key
