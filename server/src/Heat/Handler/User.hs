{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Heat.Handler.User (putUserR, getUserR, getUserCrudR) where

--
-- External imports
--
import GHC.Generics (Generic)
-- import Data.Strings (strTail)
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
import Data.Aeson.TH

--
-- Heat imports
--
import Heat.Settings (AppSettings(..))
import Heat.Foundation (appSettings, Handler)
import Heat.Utils.JWT (jsonToToken)
import Heat.Data.UserInfo (UserInfo (..))
import Heat.Data.Role (UserRole(..))
import Heat.Utils.Password (authHashPassword, authValidatePassword)
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
    fieldLabelModifier = drop 1
  } ''CreateUser)
  
data UserIdentity = UserIdentity
  { userid :: Key User
  } deriving (Generic, Show)

instance ToJSON UserIdentity

-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
putUserR :: Handler ()
putUserR = do
  newUser <- requireCheckJsonBody :: Handler CreateUser
  appset <- appSettings <$> getYesod
  hpwd <- liftIO $ authHashPassword (passwordCost appset) (cpassword newUser)
  key <- runDB $ insert400 $ User (cusername newUser) (decodeUtf8 hpwd) (crole newUser) (clevel newUser) (cemail newUser)
  sendResponseStatus created201 $ toJSON $ UserIdentity key

-- |Get a specific user given the userid, if no userid is given return with a
-- list of all users.
getUserR :: Handler Value -- ^The response
getUserR = do
  returnJson $ toJSON $ RetrieveUser "tomas" Simple 1 "tomas@stenlund.cc"

-- |User body description, comes with the CRUD operations
data RetrieveUser = RetrieveUser
  { rusername :: Text        -- ^The username of the user
  , rrole :: UserRole        -- ^The role of the user
  , rlevel :: Int            -- ^The level of the user within its role
  , remail :: Text           -- ^The email address to the user
  } deriving (Generic, Show)

$(deriveJSON defaultOptions {
     fieldLabelModifier = drop 1
     } ''RetrieveUser)

getUserCrudR :: UserId
             -> Handler Value
getUserCrudR uid = do
  user <- runDB $ get404 uid
  returnJson $ toJSON $ RetrieveUser { rusername = userUsername user,
                                     rrole = userRole user,
                                     rlevel = userLevel user,
                                     remail = userEmail user}
