-- | This module exports a pure HTML function to render a consistent footer throughout the app.
module Slip.Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Slip.Component.HTML.Utils (css)
  
footer :: forall i p. HH.HTML i p
footer = 
  HH.div
  [ css "class-center" ]
  [ HH.p       
    [ ]
    [ HH.a
      [ HP.href ""]
      [ HH.text "SLIP Administration Portal"]
    ]
  , HH.p
    [ ]
    [ HH.a
      [ HP.href ""]
      [ HH.text "Tomas Stenlund, Sundsvall"]
    ]
  ]
  
  
