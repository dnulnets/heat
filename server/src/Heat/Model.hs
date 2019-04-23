{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Heat.Model where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.TH
import Heat.Data.Role

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    password Text
    role UserRole
    level Int
    email Text
    UniqueUserUsername username    
    deriving Show Typeable
|]
