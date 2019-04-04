-- |
-- | The Main application startup module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- | Language imports
import Prelude
import Effect (Effect)
import Effect.Aff (Aff)

-- | Halogen imports
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen (hoist)
import Halogen as H
import Halogen.HTML as HH

-- | Slip imports
import Slip (Environment, runApplication)
import Slip.Root as Root

-- | Set up our environment which we will execute in
environment ∷ Environment
environment = { userName : "Tomas Stenlund"}

-- | Hoist in our Application monad
rootComponent ∷ ∀ q i . H.Component HH.HTML q i Void Aff
rootComponent = hoist (runApplication environment) Root.component

-- | The main function
main ∷ Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI rootComponent unit body
