-- |
-- | The common child query, messages and slot for all pages
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Child (Query(..),
                   Slot,
                   Message(..)) where

-- | Language imports
import Data.Maybe (Maybe)

-- | Halogen imports
import Halogen as H

-- | Our imports
import Slip.Data.Route (Page)

-- | Messages possible to send out from all main components in the root page
data Message = GotoPage Page

-- | Query algebra for all main components in the root page
data Query a = SetUser (Maybe String → a)

-- | Slot type for the all main components in the root page, they need to implement Query and Message
type Slot = H.Slot Query Message
