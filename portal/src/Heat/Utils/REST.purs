-- |
-- | The REST utilities functions
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Utils.REST where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..), hush)
import Data.Tuple (Tuple(..))
import Data.HTTP.Method (Method(..))

import Data.Argonaut (Json, class DecodeJson, decodeJson, class EncodeJson, encodeJson)

import Control.Monad.Reader (asks)
import Control.Monad.Reader.Class (class MonadAsk)

import Effect.Aff.Class (class MonadAff, liftAff)

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH

import Routing.Duplex (print)

--
-- Our own imports
--
import Heat.Interface.Endpoint (Endpoint,
                                endpointCodec)

-- |The type for "Authorization: Bearer <token>"
newtype Authorization = Bearer String
derive instance eqToken :: Eq Authorization
derive instance ordToken :: Ord Authorization

-- |The request type
data RequestMethod 
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete
    
data RequestMethod_ a
  = Get_
  | Post_ (Maybe a)
  | Put_ (Maybe a)
  | Delete_

newtype BaseURL = BaseURL String

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
}

type RequestOptions_ a =
  { endpoint :: Endpoint
  , method :: RequestMethod_ a
}

-- |The base configuration for a REST call
defaultRequest :: BaseURL -> Maybe Authorization -> RequestOptions -> AX.Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method 
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Bearer t) -> [ AXRH.RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: AXRB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: AXRF.json
  }
  where
    Tuple method body = case method of
      Get -> Tuple GET Nothing
      Post b -> Tuple POST b
      Put b -> Tuple PUT b
      Delete -> Tuple DELETE Nothing

-- |The base configuration for a REST call
defaultRequest_ :: forall a. EncodeJson a => BaseURL -> Maybe Authorization -> RequestOptions_ a -> AX.Request Json
defaultRequest_ (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left method 
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Bearer t) -> [ AXRH.RequestHeader "Authorization" $ "Bearer " <> t ]
  , content: AXRB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: AXRF.json
  }
  where
    Tuple method body = case method of
      Get_ -> Tuple GET Nothing
      Post_ b -> Tuple POST (encodeJson <$> b)
      Put_ b -> Tuple PUT (encodeJson <$> b)
      Delete_ -> Tuple DELETE Nothing

-- |Make a REST call without the need for an authentication token
mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseURL :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  baseURL <- asks _.baseURL
  response <- liftAff $ AX.request $ defaultRequest baseURL Nothing opts
  pure $ hush response.body

-- |Make a REST call without the need for an authentication token
mkRequest_
  :: forall a m r v
   . MonadAff m
  => MonadAsk { baseURL :: BaseURL | r } m
  ⇒ DecodeJson v
  ⇒ EncodeJson a
  => RequestOptions_ a
  -> m (Maybe v)
mkRequest_ opts = do
  baseURL <- asks _.baseURL
  response <- liftAff $ AX.request $ defaultRequest_ baseURL Nothing opts
  pure $ join $ hush <$> decodeJson <$> hush response.body
