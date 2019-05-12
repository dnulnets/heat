-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Data.Route (Page(..), router) where

-- | Language specifics
import Prelude
import Control.Alt ((<|>))

-- | Routing specifics
import Routing.Match (Match, lit)

-- | All possible routes
data Page = Home
          | Login
          | About
          | Users
          | Error
   
-- | Routing function that creates data types based on the URL, we only deal with home and login pages
router :: Match Page
router = home <|>
         login <|>
         about <|>
         users
  where
    home = Home <$ lit ""
    login = Login <$ lit "login"
    about = About <$ lit "about"
    users = Users <$ lit "users"
