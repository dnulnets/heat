{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Application
-- Description : The application
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the main entry point and initialization of the
-- application.
module Application where

--
-- External imports
--
import Yesod

--
-- Internal imports
--
import Heat.Settings
import Heat.Foundation
import Heat.Authenticate

--
-- The dispatcher
--
mkYesodDispatch "App" resourcesApp

-- |The main entry point for the application
appMain :: IO ()
appMain = warp 3000 App { settings = defaultSettings }
