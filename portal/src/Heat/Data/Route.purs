-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Route (Page(..), router) where

-- |Language specifics
import Prelude
import Control.Alt ((<|>))

-- |Routing specifics
import Routing.Match (Match, lit, str)

-- |All possible routes
data Page = Home
          | Login
          | User String
          | Users
          | Error

-- |Route to string
instance showPage :: Show Page where
  show Home = "home"
  show Login = "login"
  show Users = "users"
  show (User s) = "user/" <> s
  show Error = "error"

-- | Routing function that creates data types based on the URL, we only deal with home and login pages
router :: Match Page
router = home <|>
         login <|>
         users <|>
         user
  where
    home = Home <$ lit ""
    login = Login <$ lit "login"
    users = Users <$ lit "users"
    user = User <$> (lit "user" *> str)
