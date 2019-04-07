-- |
-- | The alerts for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Data.Alert (AlertType(..)) where

-- | Language specifics
import Prelude

-- | The possible alerts
data AlertType = Info | Warning | Error | Success
