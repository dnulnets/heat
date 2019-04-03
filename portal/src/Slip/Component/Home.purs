-- |
-- | The home component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Home where

-- | Language imports
import Prelude
import Data.Maybe (fromMaybe, Maybe(..))

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style)

-- | Query algebra for the component
data Query a = User (Maybe String -> a)

-- | Slot type for the componet
type Slot = H.Slot Query Void

-- | State for the component
type State = { }

-- | Initial state is no logged in user
initialState :: forall i. i -> State
initialState _ = { }

-- | The component definition
component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

-- | Render the alert
render :: forall a m. State -> H.ComponentHTML a () m
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
