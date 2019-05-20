-- |
-- | The Main application startup module
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Main where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Console (log)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA

-- | Halogen imports
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen (hoist)
import Halogen as H
import Halogen.HTML as HH

-- | Web imports
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

-- | Routing imports
import Routing.Hash (getHash, setHash, matchesWith)
import Routing.Duplex (parse)

-- | Heat imports
import Heat (Environment, runApplication)
import Heat.Root as Root
import Heat.Data.Route (router, routeCodec, Page(..))
import Heat.Utils.Request (BaseURL(..))

-- | Produce events from the browser for route changes
hashChangeProducer ∷ CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener ← DOM.eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- | Handle the change message and parse the URL to extract the page and page paramters and send it to
-- | the root page
hashChangeConsumer ∷ (∀ a. Root.Query a -> Aff (Maybe a)) → CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
      result = parse routeCodec hash
      newPage = case result of
        Left _ -> Home
        Right page -> page
  liftEffect $ log $ "New URL = '" <> hash <> "'," <> show result
  void $ query $ H.tell $ Root.GotoPage newPage
  pure Nothing

-- | Hoist in our Application monad
rootComponent ∷ ∀ i . Environment →
                H.Component HH.HTML Root.Query i Void Aff
rootComponent env = hoist (runApplication env) Root.component

-- | The main function
main ∷ Effect Unit
main = HA.runHalogenAff do
  currentUserInfo <- liftEffect $ Ref.new Nothing
  body ← HA.awaitBody
  let
    env ∷ Environment
    env = { baseURL : BaseURL "http://localhost:3000",
            userInfo : currentUserInfo }
  io ← runUI (rootComponent env) unit body
  
  -- CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
  
  void $ liftEffect $ matchesWith (parse routeCodec) \old new -> do
    liftEffect $ log $ "Router " <> show new
    when (old /= Just new) do
      liftEffect $ log $ "Router change " <> show new
      launchAff_ $ io.query $ H.tell $ Root.GotoPage new
    
