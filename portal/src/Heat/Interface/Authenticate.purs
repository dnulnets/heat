-- |
-- | The authenticate interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Interface.Authenticate (Token(..)
                                   , Authenticate(..)) where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..),
                   fromMaybe)
import Data.Either (Either(..))

import Data.Argonaut.Core (stringify)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))

-- |The token returned after an authenticate is successful
data Token = Token { userid :: String,
                     token :: String }

instance decodeJsonToken :: DecodeJson Token where
  decodeJson json = do
    obj <- decodeJson json
    userid <- obj .: "userid"
    token <- obj .: "token"
    pure $ Token { userid, token }

-- |The authentication informaion needed to be able to authenticate the user and return a token
data Authenticate = Authenticate { username :: String,
                                   password :: String }

instance encodeJsonPost :: EncodeJson Authenticate where
  encodeJson (Authenticate auth)
    = "username" := auth.username
    ~> "password" := auth.password
    ~> jsonEmptyObject
            
