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

import           Data.Typeable
import           Data.Text (Text)
import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.TH
import           Data.Aeson (Value (String), ToJSON(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    password Text
    UniqueUserUsername username    
    deriving Show Typeable
|]
  
