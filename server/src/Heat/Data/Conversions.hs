{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

-- |
-- Module      : Heat.Data.Conversions
-- Description : Different types of data conversions functionality
-- Copyright   : (c) Tomas Stenlund, 2019
-- License     : BSD-3
-- Maintainer  : tomas.stenlund@telia.com
-- Stability   : experimental
-- Portability : POSIX
-- 
-- This module contains different functions to convert from one type to another
module Heat.Data.Conversions where

--
-- External imports
--
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Int

import Data.HexString

import Database.Persist
import Database.Persist.Types
import Database.Persist.Quasi
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Class

--
-- Internal imports
--
import Heat.Data.Role
import Heat.Model

toHex::Text->HexString
toHex = hexString . encodeUtf8

toKey::ToBackendKey SqlBackend r => Text->Key r
toKey = toSqlKey . toBinary . hexString . encodeUtf8

fromKey::ToBackendKey SqlBackend r => Key r->HexString
fromKey = fromBinary . fromSqlKey

