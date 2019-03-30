-- |
-- | The alert component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Alert where

-- | Language imports
import Prelude
import Data.Maybe (fromMaybe, Maybe(..))

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style)

-- | Query algebra for the component
data Query a = Dummy (Void -> a)

-- | Slot type for the alert
type Slot = H.Slot Query Void

-- | State for the alert
type State = { message :: Maybe String }

-- | Initial state is no logged in user
initialState :: forall i. i -> State
initialState _ = { message: Nothing }

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
               [css "alert alert-danger", style "margin-top:20px"]
               [HH.text $ fromMaybe "Unknown error" state.message ]
