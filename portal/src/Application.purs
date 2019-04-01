-- |
-- | The application monad
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Application where

-- | Language imports
import Prelude
import Data.Maybe (Maybe)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Type.Equality (class TypeEquals, from)

import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)

import Data.Newtype (class Newtype, unwrap)

-- | Type of logging to perform
data LogLevel = Development | Production
derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- | The application environment
type Environment = {
  userName :: String
  }
                   
--  { logLevel :: LogLevel 
--  , baseURL :: String
--  , currentUser :: Ref (Maybe String)
-- }

-- | The application monad
newtype Application a = Application (ReaderT Environment Aff a)
derive instance newtypeApplication :: Newtype (Application a) _

-- | Run the application monad and expose the inner Aff monad
runApplication :: Environment -> Application ~> Aff
runApplication env (Application m) = runReaderT m env

-- | Derive all monad functions
derive newtype instance functorApplication :: Functor Application
derive newtype instance applyApplication :: Apply Application
derive newtype instance applicativeApplication :: Applicative Application
derive newtype instance bindApplication :: Bind Application
derive newtype instance monadApplication :: Monad Application
derive newtype instance monadEffectApplication :: MonadEffect Application
derive newtype instance monadAffApplication :: MonadAff Application

-- | ask implementation
instance monadAskApplication :: TypeEquals e Environment => MonadAsk e Application where
  ask = Application $ asks from
