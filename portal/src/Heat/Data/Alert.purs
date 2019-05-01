-- |
-- | The alerts for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Alert (AlertType(..), Alert(..)) where

-- | The possible alerts
data AlertType = Info | Warning | Error | Success

-- | An alert
-- |
-- | The type of the alert and the text message
-- |
data Alert = Alert AlertType String
