-- |
-- | The menu component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Menu where

-- Language imports
import Prelude
import Data.Maybe (Maybe(..), maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events as HE

-- Web import
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

-- DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))

-- Our own stuff
import Heat.Component.HTML.Utils (css,
                                  prop,
                                  href,
                                  maybeElem_,
                                  maybeOrElem_,
                                  maybeOrElem)
import Heat.Data.Alert as HDAL
import Heat.Data.Role (UserRole)
import Heat.Data.Route (Page(..))
import Heat.Interface.Authenticate (class ManageAuthentication,
                                    logout)
import Heat.Interface.Navigate (class ManageNavigation,
                                  gotoPage)

-- |The internal actions
data Action = SetUser (Maybe UserInfo) -- ^Used for setting the user and displaying the correct menu choices
            | DoLogout MouseEvent      -- ^Menu selection

-- |The outgoing messages
data Message = Alert HDAL.Alert -- ^The component is alerting
             | Logout           -- ^The logout message

-- | Slot type for the menu
type Slot p = ∀ q . H.Slot q Message p

-- | State for the menu, we have a user so far
data UserInfo = UserInfo { userid∷String,
                           username∷ String,
                           role∷ UserRole,
                           level∷ Int
                         }
                
type State = { user ∷ Maybe UserInfo }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i -> State
initialState _ = { user: Nothing }

-- | The component definition
component :: ∀ q m. MonadAff m
             ⇒ ManageAuthentication m
             ⇒ ManageNavigation m
             ⇒ H.Component HH.HTML q (Maybe UserInfo) Message m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
                                       receive = receive }
    }

-- | Render the menu
render ∷ ∀ m . MonadAff m
         ⇒ State → H.ComponentHTML Action () m
render state = navbar [ navbarHeader "Heat portal", navbarLeft state, navbarRight state]

-- |The navigation bar for the page
navbar∷forall p i . Array (HH.HTML p i) -> HH.HTML p i
navbar html = HH.nav [css "navbar navbar-expand-md navbar-dark bg-dark mb-4"] html

-- |The header of the navigation bar
navbarHeader∷forall p i . String -> HH.HTML p i
navbarHeader header = HH.div [css "navbar-header"] [ HH.a [css "navbar-brand", href Home] [HH.text header],
                                                     HH.button [css "navbar-toggler collapsed",
                                                                HP.type_ ButtonButton,
                                                                prop "data-toggle" "collapse",
                                                                prop "data-target" "#navbarCollapse",
                                                                HPA.expanded "false",
                                                                HPA.controls "navbarCollapse",
                                                                HPA.label "Toggle navigation"]
                                                               [HH.span [css "navbar-toggler-icon"]
                                                                []]
                                                   ]

-- |The left navigation bar
navbarLeft∷forall p . State -> HH.HTML p Action
navbarLeft state = HH.div [css "navbar-collapse collapse", HP.id_ "navbarCollapse"] [HH.ul [css "navbar-nav mr-auto"]
                                                                                     $ maybeElem_ state.user itemUsers <>
                                                                                     maybeOrElem_ state.user itemLogin itemLogout]

-- |The right navigation bar
navbarRight∷forall p . State -> HH.HTML p Action
navbarRight state = HH.a [css "navbar-text",
                          href $ maybe Login (\(UserInfo u) -> User u.userid) state.user]
                          $ maybeOrElem state.user (HH.text "Not logged in") (\(UserInfo u)->HH.text $ "Logged in as " <> u.username)

itemUsers∷forall p i . HH.HTML p i
itemUsers = HH.li [css "nav-item active"] [HH.a
                                          [css "nav-link",
                                           href $ User ""]
                                          [HH.text "Users"]]

itemLogin∷forall p . HH.HTML p Action
itemLogin = HH.li [css "nav-item"] [HH.a [css "nav-link", href Login] [HH.text "Login"]]

itemLogout∷forall p . HH.HTML p Action
itemLogout = HH.li [css "nav-item"] [HH.a [css "nav-link", href Home, HE.onClick (\a->Just $ DoLogout a)] [HH.text "Logout"]]

-- |Converts external input to internal actions for the component
receive∷Maybe UserInfo->Maybe Action
receive v = Just $ SetUser v

-- | Handles all actions for the menu component
handleAction ∷ ∀ m . MonadAff m
               ⇒ ManageAuthentication m
               ⇒ ManageNavigation m
               ⇒ Action → H.HalogenM State Action () Message m Unit

-- |SetUser UserInfo => Sets the user (username, role and level), to be used to be able to determine what menu choices to show
handleAction (SetUser u) = do
  state <- H.get
  H.put $ state { user = u }

-- |Logs out the user
handleAction (DoLogout me) = do
  H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent me
  logout
  gotoPage Login
  H.raise $ Logout
  H.raise $ Alert $ HDAL.Alert HDAL.Info "Logout successful!"
  
handleAction _ = do
  H.liftEffect $ log "Select done"
