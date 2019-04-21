{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Application where

import Yesod

import Heat.Settings
import Heat.Foundation
import Heat.Authenticate

--
-- The dispatcher
--
mkYesodDispatch "App" resourcesApp

--
-- The application main
--
appMain :: IO ()
appMain = warp 3000 App { settings = defaultSettings }
