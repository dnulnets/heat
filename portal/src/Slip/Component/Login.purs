-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Login where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..),
                   fromMaybe)
import Data.Either (Either(..))

import Data.Argonaut.Core (stringify)
import Data.Argonaut (class EncodeJson, class DecodeJson, Json,
                      encodeJson, fromArray, decodeJson,
                      jsonEmptyObject, (~>), (~>?), (:=),
                      (:=?), (.:), (.:?), (.!=))

import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.StatusCode as AXS

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

-- | Our own stuff
import Slip.Child as Child
import Slip.Component.HTML.Utils (css, style)
import Slip.Data.Alert as DAL

-- | State for the component
type State = { username∷Maybe String,
               password∷Maybe String}

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { username : Nothing,
                   password : Nothing }

-- | Internal form actions
data Action = Submit
            | Input (State→State)

-- | The component definition
component ∷ ∀ q i m .
            MonadAff m ⇒
            H.Component HH.HTML q i Child.Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Render the alert
render ∷ ∀ m . State → H.ComponentHTML Action () m
render state = HH.div
               [css "container", style "margin-top:20px"]
               [HH.div
                [css "row"]
                [HH.div
                 [css "col-md-12"]
                 [HH.div
                  [css "col-md-3 col-md-offset-1"]
                  [HH.h2
                   []
                   [HH.text "Login"],
                   HH.form
                   []
                   [HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "username"][HH.text "Username"],
                     HH.input [css "form-control", HP.name "username", HP.type_ HP.InputText,
                               HE.onValueChange \v -> Just $ Input (\st -> st { username = Just v })]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "password"][HH.text "Password"],
                     HH.input [css "form-control", HP.name "password", HP.type_ HP.InputPassword,
                               HE.onValueChange \v -> Just $ Input (\st  -> st { password = Just v })]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.button [css "btn btn-primary", HP.type_ HP.ButtonButton,
                                HE.onClick \e -> Just $ Submit] [HH.text "Login"]
                    ]
                   ]
                  ]
                 ]
                ]
               ]

-- |The token returned after an authenticate is successful
data Token = Token { token :: String }

instance decodeJsonToken :: DecodeJson Token where
  decodeJson json = do
    obj <- decodeJson json
    token <- obj .: "token"
    pure $ Token { token }

-- |The authentication informaion needed to be able to authenticate the user and return a token
data Authenticate = Authenticate { username :: String,
                                   password :: String }

instance encodeJsonPost :: EncodeJson Authenticate where
  encodeJson (Authenticate auth)
    = "username" := auth.username
    ~> "password" := auth.password
    ~> jsonEmptyObject
            
-- | Handles all actions for the login component
handleAction ∷ ∀ m .
               MonadAff m ⇒
               Action → H.HalogenM State Action () Child.Message m Unit

-- | Submit => Whenever the Login button is pressed, it will generate a submit message
handleAction Submit = do
  
  state <- H.get

  -- Try to authenticate us
  result <- H.liftAff $ AX.post AXRF.json "http://localhost:3000/authenticate"
            (AXRB.json (encodeJson $ Authenticate { username: fromMaybe "" state.username
                                                  , password: fromMaybe "" state.password}))

  case result.body of
    Left err -> do
      H.liftEffect $ log $ "POST /authenticate response failed to decode: " <> AX.printResponseFormatError err
      H.raise (Child.Alert DAL.Error "Unable to login, invalid response from server")    
    Right json -> do
      case result.status of
        AXS.StatusCode 200 -> do
          H.liftEffect $ log $ "POST /authenticate response: " <> stringify json
          H.raise (Child.Alert DAL.Info "Login succesful!")    
        otherwise -> do
          H.raise (Child.Alert DAL.Error "Unable to login, wrong username or password")
          
      H.liftEffect $ log $ "POST /authenticate response: " <> stringify json
      H.liftEffect $ log $ "Result = " <> (show result.status)

-- | Input f => Whenever the textbox entry is done, i.e. by leaving the box or pressing another control it generates a
-- | Input f message, where f is the function that operatos on the state to save the new value.
handleAction (Input f) = do
  H.liftEffect $ log $ "Setinput"
  state <- H.get
  H.put $ f state

klapp::Json -> Maybe Token
klapp j = case decodeJson j of
  Left e -> Nothing
  Right v ->  Just v
    
