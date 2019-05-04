-- |
-- | The REST request utilities functions
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Utils.Request where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..),
                    hush)
import Data.Tuple (Tuple(..))
import Data.HTTP.Method (Method(..))

import Data.Argonaut (Json,
                      class DecodeJson,
                      decodeJson,
                      class EncodeJson,
                      encodeJson)

import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk)

import Effect.Aff.Class (class MonadAff,
                         liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.StatusCode as AXS

import Routing.Duplex (print)

--
-- Our own imports
--
import Heat.Interface.Endpoint (Endpoint,
                                endpointCodec)
import Heat.Interface.Authenticate (Token(..))

-- |The type for "Authorization: Bearer <token>"
newtype Authorization = Bearer String
derive instance eqToken :: Eq Authorization
derive instance ordToken :: Ord Authorization

-- |The request type
data RequestMethod a
  = Get
  | Post (Maybe a)
  | Put (Maybe a)
  | Delete

-- |The base URL for the REST API
newtype BaseURL = BaseURL String

-- |The base configuration for a REST call
defaultRequest ∷ ∀ a. EncodeJson a ⇒ BaseURL 
               → Endpoint
               → RequestMethod a
               → Maybe Authorization
               → AX.Request Json
defaultRequest (BaseURL baseUrl) ep reqm auth =
  { method: Left method
  , url: baseUrl <> print endpointCodec ep
  , headers: case auth of
      Nothing -> []
      Just (Bearer t) -> [ AXRH.RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: AXRB.json <$> encodeJson <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: AXRF.json
  }
  where
    Tuple method body = case reqm of
      Get -> Tuple GET Nothing
      Post b -> Tuple POST b
      Put b -> Tuple PUT b
      Delete -> Tuple DELETE Nothing

-- |Makes a request to the backend and return with status and result
mkRequest ∷ ∀ a m r v. MonadAff m
            ⇒ MonadAsk { baseURL :: BaseURL | r } m
            ⇒ DecodeJson v
            ⇒ EncodeJson a
            ⇒ Endpoint
            → RequestMethod a
            → m (Tuple AXS.StatusCode (Maybe v))
mkRequest ep rm = do
  baseURL <- asks _.baseURL
  response <- liftAff $ AX.request $ defaultRequest baseURL ep rm Nothing 
  pure $ Tuple response.status $ join $ hush <$> decodeJson <$> hush response.body

-- |Makes a request to the backend and return with status
mkRequest_ ∷ ∀ a m r v. MonadAff m
            ⇒ MonadAsk { baseURL :: BaseURL | r } m
            ⇒ DecodeJson v
            ⇒ EncodeJson a
            ⇒ Endpoint
            → RequestMethod a
            → m AXS.StatusCode
mkRequest_ ep rm = do
  baseURL <- asks _.baseURL
  response <- liftAff $ AX.request $ defaultRequest baseURL ep rm Nothing
  pure response.status

-- |Converts a Token to an Authorization
mkAuthorization::Maybe Token->Maybe Authorization
mkAuthorization (Just (Token t)) = Just (Bearer t.token)
mkAuthorization _ = Nothing

-- |Makes a request to the backend and return with status and result
mkAuthRequest ∷ ∀ a m r v. MonadAff m
            ⇒ MonadAsk { baseURL :: BaseURL, token :: Ref (Maybe Token) | r } m
            ⇒ DecodeJson v
            ⇒ EncodeJson a
            ⇒ Endpoint
            → RequestMethod a
            → m (Tuple AXS.StatusCode (Maybe v))
mkAuthRequest ep rm = do
  ref <- asks _.token
  baseURL <- asks _.baseURL
  userInfo <- liftEffect $ Ref.read ref
  response <- liftAff $ AX.request $ defaultRequest baseURL ep rm $ mkAuthorization userInfo
  pure $ Tuple response.status $ join $ hush <$> decodeJson <$> hush response.body

-- |Makes a request to the backend and return with status and ignore result
mkAuthRequest_ ∷ ∀ a m r v. MonadAff m
            ⇒ MonadAsk { baseURL :: BaseURL, token :: Ref (Maybe Token) | r } m
            ⇒ DecodeJson v
            ⇒ EncodeJson a
            ⇒ Endpoint
            → RequestMethod a
            → m AXS.StatusCode
mkAuthRequest_ ep rm = do
  ref <- asks _.token
  baseURL <- asks _.baseURL
  userInfo <- liftEffect $ Ref.read ref
  response <- liftAff $ AX.request $ defaultRequest baseURL ep rm $ mkAuthorization userInfo
  pure $ response.status
