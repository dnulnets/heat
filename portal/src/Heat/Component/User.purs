-- |
-- | The user component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.User where

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
import Heat.Interface.User (class ManageUsers,
                            RetrieveUser(..),
                            retrieve)
import Heat.Interface.Authenticate (UserInfo,
                                    Authenticate(..),
                                    class ManageAuthentication,
                                    login)
               
-- | Slot type for all the Login component
type Slot p = ∀ q . H.Slot q Void p

-- | State for the component
type State = { userid∷Maybe String }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { userid : Nothing }

-- | Internal form actions
data Action = Submit Event
            | Input (State→State)

-- | The component definition
component ∷ ∀ r q i m o . MonadAff m
            ⇒ ManageUsers m
            ⇒ ManageNavigation m
            ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
            ⇒ H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Render the page
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
                   [HH.text "User information"]
                   ]
                  ]
                 ]
                ]

-- | Handles all actions for the login component
handleAction ∷ ∀ r m o . MonadAff m
               ⇒ ManageUsers m
               ⇒ ManageNavigation m
               ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m               
               ⇒ Action → H.HalogenM State Action () o m Unit

-- | Submit => Whenever the Login button is pressed, it will generate a submit message
handleAction (Submit event) = do
  H.liftEffect $ Event.preventDefault event
  state <- H.get
  H.put state
      
-- | Input f => Whenever the textbox entry is done, i.e. by leaving the box or pressing another control it generates a
-- | Input f message, where f is the function that operates on the state to save the new value. It is here we should
-- | perhaps check for format of the input etc.
handleAction (Input f) = do
  H.modify_ f
