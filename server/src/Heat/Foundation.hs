{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Yesod
import Yesod.Auth
import Data.Aeson (fromJSON,
                   Result(..))
--
-- Heat imports
--
import Heat.Settings (AppSettings(..))
import Heat.Utils.JWT (tokenToJson)
import Heat.Data.UserInfo (UserInfo(..))

-- |Our application type
data App = App {
  settings :: AppSettings -- ^The application settings
  }

--
-- The routes to this server
--
mkYesodData "App" $(parseRoutesFile "config/routes")

-- |Our application is a Yesod application
instance Yesod App where

  -- |We do not want any cookies or session data
  makeSessionBackend _ = return Nothing
  
  -- |Authroization checks for our routes
  isAuthorized AuthenticateR _ = return Authorized
  isAuthorized ApiR _ = return Authorized
    
--
-- Authorization interface
--
-- |Our application is a YesodAuth application
instance YesodAuth App where

  -- |Our authentication id
  type AuthId App = Text

  -- We are only publishing a REST JSON API, this is not needed but required
  -- by the Yesod API, implemented as error or empty
  loginDest _ = error ""
  logoutDest _ = error ""
  authPlugins _ = []
  authenticate _ = error ""

  -- |Check the JSON Web token and return with the user identity if it is valid
  maybeAuthId = do
    bearer <- lookupBearerAuth
    secret <- jwtSecret . settings <$> getYesod
    return $ case bearer of
      Nothing -> Nothing
      Just token ->
        case tokenToJson secret token of
          Nothing -> Nothing
          Just userinfo ->
            case fromJSON userinfo of
              Error _ -> Nothing
              Success info -> Just $ userid info
           
--
-- The rendermessage interface, needed by YesodAuth
--
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
    
getApiR :: Handler TypedContent
getApiR = do
  uid <- requireAuthId
  addHeader "myheader" "headerdata"
  selectRep $ do
    provideJson $ UserInfo "jfohfrhufri"
    
