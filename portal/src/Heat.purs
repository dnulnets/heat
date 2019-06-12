-- |
-- | The Heat Application Monad module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat(Environment,
            runApplication,
            ApplicationM) where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.List (List(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as REF
import Effect.Console (log)

import Type.Equality (class TypeEquals, from)

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)

-- Halogen imports
import Halogen as H

-- Routing imports
import Routing.Duplex (print, parse)
import Routing.Hash (setHash, getHash)

import Web.HTML (window)
import Web.HTML.Location as L
import Web.HTML.Window as Window

--
-- Our own imports
--
import Heat.Interface.Authenticate (UserInfo,
                                    class ManageAuthentication)
import Heat.Interface.User (RetrieveUser(..),
                            class ManageUsers)
import Heat.Interface.Navigate (class ManageNavigation)
import Heat.Interface.Endpoint as EP
import Heat.Data.Route (routeCodec)
import Heat.Utils.Request (BaseURL,
                           mkRequest,
                           RequestMethod (..))

-- | The application environment
type Environment = { baseURL ∷ BaseURL,
                     userInfo ∷ Ref (Maybe UserInfo) }

-- | The application monad
newtype ApplicationM a = ApplicationM (ReaderT Environment Aff a)
-- derive instance newtypeApplication :: Newtype (ApplicationM a) _

-- | Run the application monad and expose the inner Aff monad
runApplication :: Environment → ApplicationM ~> Aff
runApplication env (ApplicationM m) = runReaderT m env

-- | Derive all monad functions
derive newtype instance functorApplication ∷ Functor ApplicationM
derive newtype instance applyApplication ∷ Apply ApplicationM
derive newtype instance applicativeApplication ∷ Applicative ApplicationM
derive newtype instance bindApplication ∷ Bind ApplicationM
derive newtype instance monadApplication ∷ Monad ApplicationM
derive newtype instance monadEffectApplication ∷ MonadEffect ApplicationM
derive newtype instance monadAffApplication ∷ MonadAff ApplicationM

-- | ask implementation
instance monadAskApplication ∷ TypeEquals e Environment ⇒ MonadAsk e ApplicationM where
  ask = ApplicationM $ asks from

--
-- Add the set of functions that handles navigation in the app
--
instance manageNavigationApplicationM ∷ ManageNavigation ApplicationM where

  -- |Navigates the app using hash based routing
  gotoPage newPage = do
    H.liftEffect $ setHash $ print routeCodec newPage

--
--  Add the set of functions that handles login and logout of a user
--
instance manageAuthenticationApplicationM :: ManageAuthentication ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  login auth = do
    ref <- asks _.userInfo
    response <- mkRequest EP.Authenticate (Post (Just auth))
    
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        H.liftEffect $ REF.write Nothing ref
        pure Nothing
      Right (Tuple _ userInfo) -> do
        H.liftEffect $ REF.write userInfo ref
        pure userInfo

  -- |Log out the user
  logout = do
    ref <- asks _.userInfo
    H.liftEffect $ REF.write Nothing ref

--
--  Add the set of functions that handles login and logout of a user
--
instance manageUsersApplicationM :: ManageUsers ApplicationM where

  -- |Retrieves userinformation about the specified user
  retrieve userid = do
    response <- mkRequest (EP.User (Just userid)) (Get::RequestMethod Unit) 
    case response of
      Left err -> do
        H.liftEffect $ log $ "Error: " <> err
        pure Nothing
      Right (Tuple _ retrievedUser) -> do
        pure $ Just retrievedUser
    
  retrieveList = pure Nil
  update _ _ = pure unit
  delete _ = pure unit
  create _ = pure Nothing
