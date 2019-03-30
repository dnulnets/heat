-- |
-- | The main application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- | Language imports
import Prelude
import Effect (Effect)

-- | Halogen imports
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- | Slip imports
import Slip.Root as Root

-- | The main function
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Root.component unit body
