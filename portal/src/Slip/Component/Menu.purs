-- |
-- | The menu component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Menu where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA

-- | DOM import
import DOM.HTML.Indexed.ButtonType (ButtonType(..))

-- | Our own stuff
import Slip.Component.HTML.Utils (css, prop)

-- | Quer yalgebra for the component
data Query a = Dummy (Void → a)

-- | Slot type for the menu
type Slot = H.Slot Query Void

-- | State for the meny, we have a user so far
type State = { user ∷ Maybe String }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i -> State
initialState _ = { user: Nothing }

-- | The component definition
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

-- | Render the menu
render ∷ ∀ a m. State → H.ComponentHTML a () m
render state =
  HH.nav
  [css "navbar navbar-expand-md navbar-dark fixed-top bg-dark"]
  [HH.a
   [css "navbar-brand", HP.href "#home"]
   [HH.text "SLIP Portal"],
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
    ]
   ]  
  ]
  
