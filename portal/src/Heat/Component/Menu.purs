-- |
-- | The menu component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Menu where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..), maybe)

import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.Events as HE

-- | DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))

-- | Our own stuff
import Heat.Component.HTML.Utils (css,
                                  prop,
                                  maybeOrElem)
import Heat.Data.Role (UserRole)
import Heat.Data.Route (Page(..))

-- |The internal actions
data Action = SetUser (Maybe UserInfo) -- ^Used for setting the user and displaying the correct menu choices
            | Select Page              -- ^Menu selection

-- |The outgoing messages
data Message = GotoPage Page           -- ^Redirect to a page

-- | Slot type for the menu
type Slot p = ∀ q . H.Slot q Message p

-- | State for the menu, we have a user so far
data UserInfo = UserInfo { username∷ String,
                           role∷ UserRole,
                           level∷ Int
                         }
                
type State = { user ∷ Maybe UserInfo }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i -> State
initialState _ = { user: Nothing }

-- | The component definition
component :: ∀ q m. MonadAff m
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
render state = navbar [ navbarHeader "Heat portal", navbarLeft, navbarRight state]

-- |The navigation bar for the page
navbar∷forall p i . Array (HH.HTML p i) -> HH.HTML p i
navbar html = HH.nav [css "navbar navbar-expand-md navbar-dark bg-dark mb-4"] html

-- |The header of the navigation bar
navbarHeader∷forall p i . String -> HH.HTML p i
navbarHeader header = HH.div [css "navbar-header"] [ HH.a [css "navbar-brand", HP.href "#"] [HH.text header],
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
navbarLeft∷forall p . HH.HTML p Action
navbarLeft = HH.div [css "navbar-collapse collapse", HP.id_ "navbarCollapse"] [HH.ul [css "navbar-nav mr-auto"]
                                                                       [ itemHome,
                                                                         itemAbout]]

-- |The right navigation bar
navbarRight∷forall p . State -> HH.HTML p Action
navbarRight state = HH.a [css "navbar-text",
                    HE.onClick (\_->Just $ Select Login),
                    HP.href "#/"] [HH.text $ maybe "Not logged in" (\(UserInfo u)->"Logged in as " <> u.username) state.user]
  
itemHome∷forall p . HH.HTML p Action
itemHome = HH.li [css "nav-item active"] [HH.a
                                          [css "nav-link",
                                           HE.onClick (\_->Just $ Select Home),
                                           HP.href "#/"]
                                          [HH.text "Home"]]

itemAbout∷forall p i . HH.HTML p i
itemAbout = HH.li [css "nav-item"] [HH.a [css "nav-link", HP.href "#login"] [HH.text "Login"]]
            
-- |Converts external input to internal actions for the component
receive∷Maybe UserInfo->Maybe Action
receive v = Just $ SetUser v

-- | Handles all actions for the menu component
handleAction ∷ ∀ m . MonadAff m
               ⇒ Action → H.HalogenM State Action () Message m Unit

-- |SetUser UserInfo => Sets the user (username, role and level), to be used to be able to determine what menu choices to show
handleAction (SetUser u) = do
  state <- H.get
  H.put $ state { user = u }

handleAction (Select r) = do
  H.liftEffect $ log "Select done"
  H.raise (GotoPage r)
