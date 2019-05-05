-- |
-- | The user type for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.UserInfo (UserInfo(..)) where

-- Language import
import Prelude

-- Import Heat modules
import Heat.Data.Role (UserRole(..))

-- |The user type for the logged in user
data UserInfo = UserInfo {
  userid ∷ String,
  username ∷ String,
  role ∷ UserRole,
  level ∷ Int,
  email ∷ String
  }
