-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Login where

-- | Language imports
import Prelude

import Data.Maybe (Maybe(..),
                   fromMaybe)

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.Event.Event (Event)
import Web.Event.Event as Event

-- | Our own stuff
import Heat.Data.Alert as HDAL
import Heat.Data.Route (Page(..))
import Heat.Component.HTML.Utils (css, style)
import Heat.Interface.Navigate (class ManageNavigation,
                                gotoPage)
import Heat.Interface.Authenticate (UserInfo,
                                    Authenticate(..),
                                    class ManageAuthentication,
                                    login)

-- | Messages possible to send out from the login component
data Message = Alert HDAL.Alert           -- | The component is alerting
             | SetUser (Maybe UserInfo)   -- | A login event
               
-- | Slot type for all the Login component
type Slot p = ∀ q . H.Slot q Message p

-- | State for the component
type State = { username∷Maybe String,
               password∷Maybe String}

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { username : Nothing,
                   password : Nothing }

-- | Internal form actions
data Action = Submit Event
            | Input (State→State)

-- | The component definition
component ∷ ∀ r q i m . MonadAff m
            ⇒ ManageAuthentication m
            ⇒ ManageNavigation m
            ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
            ⇒ H.Component HH.HTML q i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Render the alert
render ∷ ∀ m . MonadAff m
         ⇒ State → H.ComponentHTML Action () m
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
                   [HE.onSubmit (Just <<< Submit)]
                   [HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "username"][HH.text "Username"],
                     HH.input [css "form-control", HP.name "username", HP.type_ HP.InputText,
                               HE.onValueChange \v -> Just $ Input (\st -> st { username = Just v})]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "password"][HH.text "Password"],
                     HH.input [css "form-control", HP.name "password", HP.type_ HP.InputPassword,
                               HE.onValueChange \v -> Just $ Input (\st  -> st { password = Just v })]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.button [css "btn btn-primary", HP.type_ HP.ButtonSubmit] [HH.text "Login"]
                    ]
                   ]
                  ]
                 ]
                ]
               ]

-- | Handles all actions for the login component
handleAction ∷ ∀ r m . MonadAff m
               ⇒ ManageAuthentication m
               ⇒ ManageNavigation m
               ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m               
               ⇒ Action → H.HalogenM State Action () Message m Unit

-- | Submit => Whenever the Login button is pressed, it will generate a submit message
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  state <- H.get
  userInfo <- login $ Authenticate { username: fromMaybe "" state.username
                                   , password: fromMaybe "" state.password}           
  case userInfo of
    Nothing → do
      H.raise (SetUser Nothing)
      H.raise (Alert (HDAL.Alert HDAL.Error "Login failed!"))
    Just u → do
      H.raise (SetUser $ Just u)
      gotoPage Home
      H.raise (Alert (HDAL.Alert HDAL.Info "Login successful!"))
      
-- | Input f => Whenever the textbox entry is done, i.e. by leaving the box or pressing another control it generates a
-- | Input f message, where f is the function that operates on the state to save the new value. It is here we should
-- | perhaps check for format of the input etc.
handleAction (Input f) = do
  H.modify_ f
