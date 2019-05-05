-- |
-- | The role data type
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Utils.Data (maybeFail) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either(Either(..))

-- |Converts a maybe to an either or and error if nothing
maybeFail :: forall a. String -> Maybe a -> Either String a
maybeFail s m = case m of
  Just mm -> Right mm
  Nothing -> Left s
