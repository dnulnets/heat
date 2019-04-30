-- |
-- | The home component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Home where

-- | Language imports
import Prelude

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Heat.Component.HTML.Utils (css, style)

-- | State for the component
type State = { }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { }

-- | The component definition
component ∷ ∀ q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

-- | Render the homepage
render ∷ ∀ a m. State → H.ComponentHTML a () m
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
                   [HH.text "Home page"]
                  ]
                 ]
                ]
               ]
