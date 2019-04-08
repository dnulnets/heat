-- |
-- | The routes for the application
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Data.Route (Page(..), router) where

-- | Language specifics
import Prelude
import Control.Alt ((<|>))
-- | Routing specifics
import Routing.Match (Match, lit, num)

-- | All possible routes
data Page = Home |
            Login |
            Error

instance showPage :: Show Page where
   show Home = "Home"
   show Login = "Login"
   show Error = "Error"
   
-- | Routing function that creates data types based on the URL, we only deal with home and login pages
router :: Match Page
router = home <|>
         login
  where
    home = Home <$ lit ""
    login = Login <$ lit "login"
