-- |
-- | The endponit module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Interface.Endpoint where

-- | Language imports
import Prelude

import Data.Maybe (Maybe)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex (RouteDuplex', path, root, segment, string, optional)

-- |The base URL for the api
newtype BaseURL = BaseURL String

-- |The nedpoint needed from the backend server
data Endpoint = Authenticate
              | User (Maybe String)

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

-- |The endpoint codec
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Authenticate": path "authenticate" noArgs
  , "User": path "user" (optional (string segment)) }
