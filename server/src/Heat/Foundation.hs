{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Heat.Foundation where

import GHC.Generics

import Data.Text (Text)
import Yesod
import Yesod.Auth
import Yesod.Auth.Message as AuthMsg
import Network.HTTP.Types ( status400, status401 )
import Network.Wai (requestHeaders)
import Data.Map as Map (fromList, (!?))
import Data.Aeson (fromJSON, Result(..))

import Heat.Settings
import Heat.Utils.JWT
import Heat.Data.UserInfo

--
-- Our application type
--
data App = App {
  settings :: AppSettings
  }

--
-- The routes to this server
--
mkYesodData "App" $(parseRoutesFile "config/routes")

--
-- Yesod interface
--
instance Yesod App where

  --
  -- No cookies
  --
  makeSessionBackend _ = return Nothing
  
  --
  -- Check the authorization of the API
  --
  isAuthorized AuthenticateR _ = return Authorized
--  isAuthorized ApiR _ = return $ Unauthorized "You must login to access this page"
  isAuthorized ApiR _ = return Authorized
    
--
-- Authorization interface
--
instance YesodAuth App where

  --
  -- The Authenticated id
  --
  type AuthId App = Text

  --
  -- We are only servicing a REST JSON API, no other so theese are not needed
  -- but required by the haskell api
  --
  loginDest _ = error ""
  logoutDest _ = error ""
  authPlugins _ = []
  authenticate _ = error ""

  --
  -- Returns with the current authenticated id if there is one
  --
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
    
