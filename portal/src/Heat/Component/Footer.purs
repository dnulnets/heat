-- |
-- | The footer component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Component.Footer where

-- | Language imports
import Prelude

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Heat.Component.HTML.Utils (css)

-- | Slot type for the componet
type Slot p = ∀ q . H.Slot q Void p

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

-- | Render the alert
render ∷ ∀ a m. State → H.ComponentHTML a () m
render state = HH.div
               [css "text-center"]
               [HH.p
                []
                [HH.a
                 []
                 [HH.text "SLIP Administration Portal"]
                ],
                HH.p
                []
                [HH.a
                 []
                 [HH.text "Tomas Stenlund, Sundsvall Sweden"]
                ]
               ]
