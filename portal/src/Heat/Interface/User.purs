-- |
-- | The authenticateuser interface
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Interface.User where

-- Language imports
import Prelude

import Data.Maybe (Maybe)
import Data.List(List)
import Data.Argonaut (class DecodeJson,
                      class EncodeJson,
                      decodeJson, jsonEmptyObject,
                      (.:),
                      (:=),
                      (:=?),
                      (~>),
                      (~>?))

import Control.Apply (lift2)

-- Halogen imports
import Halogen (HalogenM, lift)

-- The user role
import Heat.Data.Role (UserRole)

--
-- PUT USER (Create a new user)
--

-- |The body of the create user call
data CreateUser = CreateUser {
  username ∷ String,
  password ∷ String,
  role ∷ UserRole,
  level ∷ Int,
  email ∷ String }

instance encodeJsonCreateUser :: EncodeJson CreateUser where
  encodeJson (CreateUser user)
    = "username" := user.username
    ~> "password" := user.password
    ~> "role" := user.role
    ~> "level" := user.level
    ~> "email" := user.email
    ~> jsonEmptyObject

-- | The return body from a create of a user
data UserIdentity = UserIdentity {
  userid ∷ String }

instance decodeJsonUserIdentity :: DecodeJson UserIdentity where
  decodeJson json = do
    obj ← decodeJson json
    userid ← obj .: "userid"
    pure $ UserIdentity { userid }

--
-- GET USER (Fetch a specific user or the entire list of users)
--
    
-- |The body of the response for fetching a user
data RetrieveUser = RetrieveUser {
  userid ∷ String,
  username ∷ String,
  role ∷ UserRole,
  level ∷ Int,
  email ∷ String }

instance decodeJsonRetrieveUser :: DecodeJson RetrieveUser where
  decodeJson json = do
    obj ← decodeJson json
    userid ← obj .: "userid"
    username ← obj .: "username"
    role ← obj .: "role"
    level ← obj .: "level"
    email ← obj .: "email"
    pure $ RetrieveUser { userid, username, role, level, email }

--
-- POST USER (Update user)
--
    
-- |The body of an update user call
data UpdateUser = UpdateUser {
  username ∷ Maybe String,
  password ∷ Maybe String,
  role ∷ Maybe UserRole,
  level ∷ Maybe Int,
  email ∷ Maybe String }

instance encodeJsonUpdateUser :: EncodeJson UpdateUser where
  encodeJson (UpdateUser user)
    = "username" :=? user.username
    ~>? "password" :=? user.password
    ~>? "role" :=? user.role
    ~>? "level" :=? user.level
    ~>? "email" :=? user.email
    ~>? jsonEmptyObject

-- |The class for user handling
class Monad m ⇐ ManageUsers m where
  retrieve∷String               -- ^The user identity
    →m (Maybe RetrieveUser)     -- ^The retrieved user
  retrieveList∷m (List RetrieveUser) -- ^The list of retrieved users
  update∷String                 -- ^The user identity
    →UpdateUser                 -- ^The user data to be updated
    →m Unit
  delete∷String                 -- ^The user identity
    →m Unit
  create∷CreateUser             -- ^The user data
    →m (Maybe UserIdentity)     -- ^The user identity
  
-- |Avoid lift in the components
instance manageUsersHalogenM :: ManageUsers m ⇒ ManageUsers (HalogenM st act slots msg m) where
  retrieve = lift <<< retrieve
  retrieveList = lift retrieveList
  create = lift <<< create
  delete = lift <<< delete
  update userid = lift <<< update userid

