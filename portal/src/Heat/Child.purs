-- |
-- | The common child query, messages and slot for all pages
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Child (Slot,
                   Message(..)) where

-- | Language imports

-- | Halogen imports
import Halogen as H

-- | Our imports
import Heat.Data.Route (Page)
import Heat.Data.Alert (AlertType(..))

-- | Messages possible to send out from all main components in the root page
data Message = GotoPage Page |           -- | Goto to the specified page
               Alert AlertType String    -- | The component is alerting

-- | Slot type for all the main components in the root page, they need to implement Message
type Slot p = ∀ q . H.Slot q Message p

--
-- Scratch area
--
-- Old definition of Slot
--
-- type Slot = H.Slot Query Message
