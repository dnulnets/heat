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
-- import Data.Either (Either(..), hush)
import Data.Tuple (Tuple(..))

-- import Data.Argonaut (encodeJson, decodeJson)

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as REF
-- import Effect.Console (log)

import Type.Equality (class TypeEquals, from)

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)

-- import Affjax as AX
-- import Affjax.ResponseFormat as AXRF
-- import Affjax.RequestBody as AXRB
-- import Affjax.StatusCode as AXS

import Halogen as H

--
-- Our own imports
--
import Heat.Interface.Authenticate (Token,
                                    class ManageAuthentication)
import Heat.Utils.Request (BaseURL,
                           mkRequest,
                           RequestMethod (..))
import Heat.Interface.Endpoint as EP

-- | The application environment
type Environment = { baseURL ∷ BaseURL,
                     token ∷ Ref (Maybe Token) }

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
--  Add the set of functions that handles login and logout of a user
--
instance manageAuthenticationApplicationM :: ManageAuthentication ApplicationM where

  -- |Tries to login the user and get a token from the backend that can be used for future
  -- calls
  login auth = do
    ref <- asks _.token
    (Tuple _ token) <- mkRequest EP.Authenticate (Post (Just auth))
    H.liftEffect $ REF.write token ref
    pure token

  -- |Logs out the user
  logout = do
    ref <- asks _.token
    H.liftEffect $ REF.write Nothing ref
    
