{-# LANGUAGE OverloadedStrings #-}

module Heat.Settings (AppSettings(..),
                      defaultSettings) where

import Data.Text (Text)

data AppSettings = AppSettings {
  jwtSecret :: Text
  }

defaultSettings = AppSettings {
  jwtSecret = "mandelmassa"
  }
