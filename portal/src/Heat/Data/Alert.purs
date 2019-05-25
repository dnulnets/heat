-- |
-- | The alerts for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Alert (AlertType(..), Alert(..)) where

-- | Language imports
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- | The possible alerts
data AlertType = Info | Warning | Error | Success
derive instance genericAlertType :: Generic AlertType _
instance showAlertType :: Show AlertType where
  show = genericShow
  
-- | An alert
-- |
-- | The type of the alert and the text message
-- |
data Alert = Alert AlertType String
derive instance genericAlert :: Generic Alert _
instance showAlert :: Show Alert where
  show = genericShow
