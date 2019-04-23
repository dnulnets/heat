{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Heat.Model
-- Description : The persisten model
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains the definition of the data model that are persistent.
module Heat.Model where

--
-- External imports
--
import Data.Typeable (Typeable)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.TH

--
-- Internal imports
--
import Heat.Data.Role

--
-- The persist model
--
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
