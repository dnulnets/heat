{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functions to create and verify JSON Web Tokens
-- and to extract and insert the unregistered claim as a JSON structure
-- where you can store relevant information for your application
module Heat.Utils.JWT (jsonToToken,
                       tokenToJson) where

--
-- External Library imports
--
import Data.Text (Text)
import Web.JWT as JWT
import Data.Map as Map (fromList, (!?))
import Data.Aeson (Value)

-- | The name of the unregistered claim in the JSON Web Token
jwtKey :: Text -- ^ The name of the key
jwtKey = "info"

-- | Create a token out of a given JSON 'Value'
jsonToToken :: Text  -- ^ The secret used for signing
            -> Value -- ^ The JSON value to use as a unregistered claim
            -> Text  -- ^ The token
jsonToToken jwtSecret userId =
  encodeSigned (JWT.hmacSecret jwtSecret)
    mempty {typ = Just "JWT", alg = Just HS256}
    mempty {unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)]}

-- | Extract a JSON 'Value' out of a token
tokenToJson :: Text        -- ^ The secret to verify the signature with
            -> Text        -- ^ The token
            -> Maybe Value -- ^ The JSON value
tokenToJson jwtSecret token = do
  jwt <- JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) token
  unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? jwtKey
