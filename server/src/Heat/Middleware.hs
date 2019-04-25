{-# LANGUAGE OverloadedStrings #-}

module Heat.Middleware (corsified) where

import Network.Wai                       (Middleware)
-- import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors       (CorsResourcePolicy(..), cors)

corsified :: Middleware
corsified = cors (const $ Just corsPolicy)

-- | CORS resource policy to be used with 'corsified' middleware.
corsPolicy :: CorsResourcePolicy
corsPolicy = CorsResourcePolicy {
    corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
}
