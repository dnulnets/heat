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
module Heat.Handler.Authenticate (postAuthenticateR) where

--
-- External imports
--
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.HexString (HexString)
import Data.ByteString (ByteString)
import Data.Time.Clock.System (getSystemTime,
                               SystemTime(..))

--
-- Internal imports
--
import Yesod
import Heat.Model
--
-- Heat imports
--
import Heat.Settings (AppSettings(..))
import Heat.Foundation (appSettings, Handler, maybeAuthId)
import Heat.Utils.JWT (jsonToToken)
import Heat.Data.Conversions (keyToHex)
import Heat.Utils.Password (authHashPassword, authValidatePassword)
import Heat.Interface.Authenticate (Authenticate(..), Token (..))
  
-- |Authenticate the user and create a JSON Web Token that is returned so it can be used
-- for following calls
postAuthenticateR :: Handler Value
postAuthenticateR = do
  authId <- maybeAuthId
  liftIO $ print $ authId
  auth <- requireCheckJsonBody :: Handler Authenticate
  dbuser <- runDB $ getBy $ UniqueUserUsername $ username auth
  seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime
  appset <- appSettings <$> getYesod
  let secret = tokenSecret appset
      length = tokenExpiration appset
    in case dbuser of
         Just (Entity userId user) | authValidatePassword (userPassword user) (password auth) -> do
                                       token <- return $ jsonToToken secret seconds length $ toJSON userId
                                       returnJson $ Token (keyToHex userId) token
         _ -> notAuthenticated
