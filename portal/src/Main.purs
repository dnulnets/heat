-- |
-- | The main application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref

-- | Halogen imports
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen (liftEffect, hoist)
import Halogen as H
import Halogen.HTML as HH

-- | Slip imports
import Slip.Application as APP
import Slip.Root as Root

-- | The main function
main :: Effect Unit
main = HA.runHalogenAff do
  
  body <- HA.awaitBody
  currentUser <- liftEffect $ Ref.new Nothing

  let
    
    baseURL = "http://localhost"
    logLevel = APP.Development
    
    environment :: APP.Environment
    environment = { currentUser, baseURL, logLevel }

    rootComponent :: forall q i. H.Component HH.HTML q i Void Aff
    rootComponent = hoist (APP.runApplication environment) Root.component

  runUI rootComponent unit body
