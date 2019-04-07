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
import Slip.Data.Alert as A

-- | Query algebra for the component
data Query a = Alert A.AlertType String a

-- | Input to the component, is an alert or nothing
type Input = Maybe A.Alert

-- | Internal actions
data Action = HandleInput Input

-- | Slot type for the alert
type Slot = H.Slot Query Void

-- | State for the alert
type State = { message ∷ Maybe String,
               level ∷ Maybe A.AlertType,
               displayed ∷ Boolean }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { message : Nothing,
                   level : Nothing,
                   displayed : false }

-- | The component definition
component ∷ ∀ o m .
            MonadAff m ⇒
            H.Component HH.HTML Query Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
                                       receive = \v → Just $ HandleInput v,
                                       handleQuery = handleQuery }
    }

-- | Render the alert
render ∷ ∀ m. State → H.ComponentHTML Action () m
render state = maybeElem state.message $ message $ fromMaybe A.Info state.level

-- | Display the message
message ∷ ∀ p i . A.AlertType → String → HH.HTML p i
message level msg = HH.div
            [css $ "alert " <> alertClass level, style "margin-top:20px"]
            [HH.text msg ]

-- | Bootstrap alert message
alertClass::A.AlertType→String
alertClass A.Info = "alert-info"
alertClass A.Warning = "alert-warning"
alertClass A.Error = "alert-danger"
alertClass A.Success = "alert-success"

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
  H.liftEffect $ log $ "Alert has displayed = " <> show state.displayed
  if state.displayed
    then
      H.put state { message = Nothing, displayed = false }
    else
      H.put state { displayed = isJust state.message }

-- |
-- | Handle the Queries for this component
-- |
handleQuery ∷ ∀ o m a .
              MonadAff m ⇒
              Query a → H.HalogenM State Action () o m (Maybe a)

-- |
-- | Alert AlertType String => Set the message that is to be render to the user
-- |
handleQuery (Alert alrt msg a) = do
    state <- H.get
    H.liftEffect $ log $ msg
    H.put state {message = Just msg, level = Just alrt, displayed = false}
    pure (Just a)
