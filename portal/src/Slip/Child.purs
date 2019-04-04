-- |
-- | The common child query for all pages
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Child (Query(..),
                   Slot) where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))

-- | Halogen imports
import Halogen as H

-- | Query algebra for all main components that we route to
data Query a = SetUser (Maybe String → a)

-- | Slot type for the all main components
type Slot = H.Slot Query Void
