-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Route (Page(..), router, routeCodec) where

-- |Language specifics
import Prelude hiding ((/))

import Control.Alt ((<|>))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

-- |Routing specifics
import Routing.Match (Match, lit, str)

import Routing.Duplex (RouteDuplex', as, path, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

-- |All possible routes
data Page = Home
          | Login
          | User String
          | Error

derive instance genericRoute :: Generic Page _
derive instance eqRoute :: Eq Page
derive instance ordRoute :: Ord Page
instance showPage :: Show Page where
  show = genericShow

-- | Routing function that creates data types based on the URL, we only deal with home and login pages
router :: Match Page
router = home <|>
         login <|>
         user
  where
    home = Home <$ lit ""
    login = Login <$ lit "login"
    user = User <$> (lit "user" *> str)

-- | Bidirectional parsing and unparsing
routeCodec :: RouteDuplex' Page
routeCodec = root $ sum
  { "Home": noArgs
  , "Login": "login" / noArgs
  , "Error": "error" / noArgs
  , "User": "user" / string segment
  }
