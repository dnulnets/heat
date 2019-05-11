-- |
-- | The menu component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Menu where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

-- | DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))

-- | Our own stuff
import Heat.Component.HTML.Utils (css, prop, maybeElem)
import Heat.Interface.Authenticate as HIA
import Heat.Data.Role (UserRole(..))

-- | Slot type for the menu
type Slot p = ∀ q . H.Slot q Void p

-- | State for the menu, we have a user so far
data UserInfo = UserInfo { username∷ String,
                           role∷ UserRole,
                           level∷ Int
                         }
                
type State = { user ∷ Maybe UserInfo }

-- |The internal actions
data Action = SetUser (Maybe UserInfo)

-- | Initial state is no logged in user
initialState ∷ ∀ i. i -> State
initialState _ = { user: Nothing }

-- | The component definition
component :: ∀ q o m. MonadAff m
             ⇒ H.Component HH.HTML q (Maybe UserInfo) o m
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
                                       receive = \v → Just $ SetUser v }
    }

-- | Render the menu
render ∷ ∀ m . MonadAff m
         ⇒ State → H.ComponentHTML Action () m
render state =
  HH.nav
  [css "navbar navbar-expand-md navbar-dark fixed-top bg-dark"]
  [HH.a
   [css "navbar-brand", HP.href "#home"]
   [HH.text "Heat Portal"],
   HH.button
   [css "navbar-toggler", HP.type_ ButtonButton, HPA.controls "navbarCollapse", HPA.expanded "false",
    HPA.label "Toggle navigation", prop "data-toggle" "collapse", prop "data-target" "#navbarCollapse"]
   [HH.span
    [css "navbar-toggler-icon"]
    []
   ],
   HH.div
   [css "collapse navbar-collapse"]
   [HH.ul
    [css "navbar-nav mr-auto"]
    [HH.li
     [css "nav-item active"]
     [HH.a
      [css "nav-link", HP.href "#login"]
      [HH.text "Home",
       HH.span
       [css "sr-only"]
       [HH.text "(current)"]
      ]
     ],
     HH.li
     [css "nav-item"]
     [HH.a
      [css "nav-link", HP.href "#login/2178342/tomas"]
      [HH.text "Users"]
     ]    
    ],
    HH.p
    [css "navbar-text navbar-right"]
    [ maybeElem state.user (\(UserInfo u) → HH.text $ "Signed in as " <> u.username) ]
   ]  
  ]

-- | Handles all actions for the login component
handleAction ∷ ∀ o m . MonadAff m
               ⇒ Action → H.HalogenM State Action () o m Unit
handleAction (SetUser u) = do
  state <- H.get
  H.put $ state { user = u }
