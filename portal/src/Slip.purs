-- |
-- | The Slip Application Monad module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip(Environment,
            runApplication,
            ApplicationM) where

-- | Language imports
import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)

import Type.Equality (class TypeEquals, from)

import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Reader.Trans (ReaderT)

-- | The application environment
type Environment = {
  userName ∷ String
  }

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
