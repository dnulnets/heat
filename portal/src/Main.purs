-- |
-- | The main application
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
import Application as APP
import Slip.Root as Root

-- | The main function
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
        
    environment :: APP.Environment
    environment = { userName : "Tomas Stenlund"}

    rootComponent :: forall q i. H.Component HH.HTML q i Void Aff
    rootComponent = hoist (APP.runApplication environment) Root.component

  runUI rootComponent unit body
