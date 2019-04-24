{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Heat.Foundation
-- Description : The yesod application definition
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the yesod application definition
module Heat.Foundation (Handler,
                        App(..),
                        resourcesApp,
                        Route (..),
                        getApiR) where

--
-- External imports
--
import Data.Text (Text)
import Data.Time.Clock.System (getSystemTime, SystemTime(..))
import Yesod
import Yesod.Core.Types (Logger)
import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Database.Persist
import Yesod.Auth
import Data.Aeson (fromJSON,
                   Result(..))
import Network.HTTP.Client.Conduit (Manager)
--
-- Heat imports
--
import Heat.Settings (AppSettings(..))
import Heat.Utils.JWT (tokenToJson)
import Heat.Data.UserInfo (UserInfo(..))
import Heat.Model

-- |Our application type
data App = App {
  appSettings :: AppSettings -- ^The application settings
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger  }

--
-- The routes to this server
--
mkYesodData "App" [parseRoutes|
/api ApiR GET
/user UserR PUT GET
/user/#UserId UserCrudR GET POST
/authenticate AuthenticateR POST
|]

-- |Our application is a Yesod application
instance Yesod App where

  -- |We do not want any cookies or session data
  makeSessionBackend _ = return Nothing
  
  -- |Authroization checks for our routes
  isAuthorized AuthenticateR _ = return Authorized
  isAuthorized UserR _ = return Authorized
  isAuthorized ApiR _ = return Authorized
  isAuthorized (UserCrudR _) _ = return Authorized
  
--
-- Authorization interface
--
-- |Our application is a YesodAuth application
instance YesodAuth App where

  -- |Our authentication id
  type AuthId App = UserId

  -- We are only publishing a REST JSON API, this is not needed but required
  -- by the Yesod API, implemented as error or empty
  loginDest _ = error ""
  logoutDest _ = error ""
  authPlugins _ = []
  authenticate _ = error ""

  -- |Check the JSON Web token and return with the user identity if it is valid
  maybeAuthId = do
    bearer <- lookupBearerAuth
    seconds <- liftIO $ fromIntegral . systemSeconds <$> getSystemTime    
    secret <- tokenSecret . appSettings <$> getYesod
    return $ case bearer of
      Nothing -> Nothing
      Just token ->
        case tokenToJson secret seconds token of
          Nothing -> Nothing
          Just info ->
            case fromJSON info of
              Error _ -> Nothing
              Success uid -> Just $ uid

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodAuthPersist App

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

--
-- The rendermessage interface, needed by YesodAuth
--
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
getApiR :: Handler TypedContent
getApiR = do
  uid <- requireAuthId
  u <- requireAuth
  liftIO $ print $ case u of
    Entity _ e -> show e
  addHeader "myheader" "headerdata"
  selectRep $ do
    provideJson $ UserInfo 1
