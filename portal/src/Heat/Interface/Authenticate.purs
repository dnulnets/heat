-- |
-- | The authenticate interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Interface.Authenticate (Token(..)
                                   , Authenticate(..)
                                   , class ManageAuthentication, login, logout) where

-- Language imports
import Prelude

import Data.Maybe (Maybe)
import Data.Argonaut (class DecodeJson,
                      class EncodeJson,
                      decodeJson, jsonEmptyObject,
                      (.:),
                      (:=),
                      (~>))

-- Halogen imports
import Halogen (HalogenM, lift)

-- |The token returned after an authenticate is successful
data Token = Token { userid ∷ String,
                     token ∷ String }

instance decodeJsonToken :: DecodeJson Token where
  decodeJson json = do
    obj ← decodeJson json
    userid ← obj .: "userid"
    token ← obj .: "token"
    pure $ Token { userid, token }

-- |The authentication informaion needed to be able to authenticate the user and return a token
data Authenticate = Authenticate { username ∷ String,
                                   password ∷ String }

instance encodeJsonPost :: EncodeJson Authenticate where
  encodeJson (Authenticate auth)
    = "username" := auth.username
    ~> "password" := auth.password
    ~> jsonEmptyObject
            
-- |The class for authentication
class Monad m ⇐ ManageAuthentication m where

  -- |Tries to log in and returns with a token if succesful
  login∷Authenticate     -- ^Authentication information
       →m (Maybe Token)  -- ^Token
       
  -- |Logs out the user
  logout∷m Unit
  
-- |Avoid lift in the components
instance manageAuthenticationHalogenM :: ManageAuthentication m => ManageAuthentication (HalogenM st act slots msg m) where
  login = lift <<< login  
  logout = lift logout
