-- |
-- | The footer component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Footer where

-- | Language imports
import Prelude
import Data.Maybe (fromMaybe, Maybe(..))

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style)

-- | Query algebra for the component
data Query a = Dummy (Void → a)

-- | Slot type for the componet
type Slot = H.Slot Query Void

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
