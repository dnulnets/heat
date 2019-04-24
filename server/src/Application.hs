{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

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
module Application (appMain) where

--
-- External imports
--
import Control.Monad.Logger (runLoggingT)

import Database.Persist.Sql (runSqlPool, runMigration)
import Database.Persist.Postgresql (createPostgresqlPool, PostgresConf(..))

import Yesod
import Yesod.Default.Config2 (makeYesodLogger)
import Network.HTTP.Client.Conduit (Manager, newManager)

import System.Log.FastLogger (defaultBufSize,
                              newStdoutLoggerSet,
                              toLogStr)
--
-- Internal imports
--
import Heat.Model (migrateAll)
import Heat.Settings (defaultSettings, AppSettings(..))
import Heat.Foundation (App(..),
                        Route(..),                        
                        resourcesApp,                        
                        getApiR)

--
-- Our routes
--
import Heat.Authenticate (postAuthenticateR)
import Heat.Handler.User (putUserR, getUserR, getUserCrudR, postUserCrudR)

--
-- The dispatcher
--
mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

  let mkFoundation appConnPool = App {..}
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger

  pool <- flip runLoggingT logFunc $ createPostgresqlPool
          (pgConnStr $ databaseConf appSettings)
          (pgPoolSize $ databaseConf appSettings)

  flip runLoggingT logFunc $ flip runSqlPool pool $ runMigration migrateAll

  return $ mkFoundation pool

-- |The main entry point for the application
appMain :: IO ()
appMain = do
  foundation <- makeFoundation defaultSettings
  warp 3000 foundation
