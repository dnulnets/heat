-- |
-- | The role data type
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Role (UserRole(..)) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either(Either(..))

import Data.Argonaut (fromString,
                      toString,
                      class DecodeJson,
                      class EncodeJson,
                      decodeJson, jsonEmptyObject,
                      (.:),
                      (:=),
                      (~>))

-- Heat imports
import Heat.Utils.Data (maybeFail)

-- |The role a user can have
data UserRole = Simple
              | Administrator
              | Device

instance encodeJsonRole :: EncodeJson UserRole where
  encodeJson role = fromString $ case role of
    Simple → "simple"
    Administrator → "administrator"
    Device → "device"

instance decodeJsonRole :: DecodeJson UserRole where
  decodeJson json = do
    a <- maybeFail "Could not read Property string." $ toString json
    case a of
      "simple" -> Right Simple
      "administrator" -> Right Administrator
      "device" -> Right Device
      _ -> Left $ "UserRole property string was not one of the" <>
                  "expected values. Value was '" <> a <> "'"

