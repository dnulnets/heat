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
module Heat.Handler.User (putUserR, getUserR, getUserCrudR, postUserCrudR, deleteUserCrudR) where

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
import Heat.Data.Conversions (toKey, keyToHex, toHex)
import Heat.Data.Role (UserRole(..))
import Heat.Utils.Password (authHashPassword, authValidatePassword)
import Heat.Interface.User
import Heat.Model

--
-- PUT and GET for the /user path
--

-- |Creates a user, check for uniqueness and hash the password. Return with the unique user identity.
putUserR :: Handler Value
putUserR = do
  newUser <- requireCheckJsonBody :: Handler CreateUser
  appset <- appSettings <$> getYesod
  hpwd <- liftIO $ authHashPassword (passwordCost appset) (cpassword newUser)
  key <- runDB $ insert400 $ User (cusername newUser) (decodeUtf8 hpwd) (crole newUser) (clevel newUser) (cemail newUser)
  sendResponseStatus created201 $ toJSON $ UserIdentity (keyToHex key)

-- |Return with a list of all users
getUserR :: Handler Value -- ^The response
getUserR = do
  users <- (runDB $ selectList [] [Asc UserId])
  returnJson $ map convert users
  where
    convert (Entity uid user) = RetrieveUser { ruserid = (keyToHex uid)
                                             , rusername = userUsername user
                                             , rrole = userRole user
                                             , rlevel = userLevel user
                                             , remail = userEmail user}

--
-- GET, POST an DELEET for /usr/<identity>
--

-- |Retrieve the specified user if it exists.
getUserCrudR :: Text            -- ^The users identity
             -> Handler Value   -- ^The response
getUserCrudR uid = do
  user <- runDB $ get404 (toKey uid)
  returnJson $ RetrieveUser { ruserid = toHex uid
                            , rusername = userUsername user
                            , rrole = userRole user
                            , rlevel = userLevel user
                            , remail = userEmail user}

-- |Updates fields for a specific user
postUserCrudR :: Text     -- ^The user identity
              -> Handler () -- ^The response
postUserCrudR uid = do
  
  user <- requireCheckJsonBody :: Handler UpdateUser
  runDB $ update (toKey uid) $
    changeField UserUsername (uusername user) <>
    changeField UserRole (urole user) <>
    changeField UserLevel (ulevel user) <>
    changeField UserEmail (uemail user)
  return ()
  
  where
    
    changeField::(PersistField a) => EntityField User a -> Maybe a -> [Update User]
    changeField field (Just value) = [field =. value]
    changeField _ Nothing  = []
    
-- |Delete the specified user
deleteUserCrudR :: Text         -- ^The users identity
                -> Handler ()   -- ^The response
deleteUserCrudR uid = do
  runDB $ delete (toKey uid::UserId)
