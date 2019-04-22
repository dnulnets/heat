{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Heat.Utils.Password
-- Description : Password handling
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains functionality to verify and hash passwords.
module Heat.Utils.Password (authHashPassword, authValidatePassword) where

--
-- External imports
--
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.ByteString (ByteString)
import Crypto.KDF.BCrypt (hashPassword,validatePassword)

-- |Validates a password by checking the hashed password in the database with the
-- user supplied password.
authValidatePassword::Text -- ^The hashed password
                    ->Text -- ^The user supplied password in clear text
                    ->Bool -- ^True if they match, otherwise false
authValidatePassword hpwd upwd = validatePassword (encodeUtf8 upwd) (encodeUtf8 hpwd) 

-- |Hashes a password.
authHashPassword :: Int           -- ^The cost of the hashing work
                 -> Text          -- ^The user supplied password in clear text
                 ->IO ByteString  -- ^The hashed password
authHashPassword cost pwd = do
  result <- hashPassword cost (encodeUtf8 pwd) :: IO ByteString
  return result
