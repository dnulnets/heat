-- |
-- | The alert component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Alert where

-- | Language imports
import Prelude
import Data.Maybe (fromMaybe, isJust, Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style, maybeElem)
import Slip.Data.Alert as AL

-- | Input to the component, is an alert or nothing
type Input = Maybe AL.Alert

-- | Internal actions
data Action = HandleInput Input

-- | Slot type for the alert, we do not have any Query therefore it looks like this
type Slot p = ∀ q . H.Slot q Void p

-- | State for the alert
type State = { alert ∷ Maybe AL.Alert,
               displayed ∷ Boolean }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { alert : Nothing,
                   displayed : false }

-- | The component definition
component ∷ ∀ q o m .
            MonadAff m ⇒
            H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
                                       receive = \v → Just $ HandleInput v }
    }

-- | Render the alert
render ∷ ∀ m. State → H.ComponentHTML Action () m
render state = maybeElem state.alert message

-- | Display the message
message ∷ ∀ p i . AL.Alert → HH.HTML p i
message (AL.Alert lvl msg) = HH.div
            [css $ "alert " <> alertClass lvl, style "margin-top:20px"]
            [HH.text msg ]

-- | Bootstrap alert message
alertClass::AL.AlertType→String
alertClass AL.Info = "alert-info"
alertClass AL.Warning = "alert-warning"
alertClass AL.Error = "alert-danger"
alertClass AL.Success = "alert-success"

-- |
-- | Handle the actions for this component:
-- |
handleAction ∷ ∀ o m .
               MonadAff m ⇒
               Action → H.HalogenM State Action () o m Unit

-- |
-- | Render => Remove the alert message if it has been rendered once, we only want it to stay for one
-- |           rendering
-- |
handleAction (HandleInput alert) = do
  H.liftEffect $ log "Alert handle input"
  state <- H.get
  H.put state { alert = alert }
