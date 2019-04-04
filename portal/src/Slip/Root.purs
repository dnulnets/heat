-- |
-- | The root page for the entire application.
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Root  where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Control.Monad.Reader.Trans (class MonadAsk, ask, asks)
import Control.Monad.Trans.Class (lift)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- | Halogen imports
import Halogen as H
import Halogen.HTML as HH

-- | Slip imports
import Slip.Component.HTML.Utils (css, style)
import Slip.Component.Menu as Menu
import Slip.Component.Alert as Alert
import Slip.Component.Footer as Footer
import Slip.Component.Login as Login

data Action = SetUser

-- | The state for the application, it will contain the logged in user among other things
type State = { user ∷ Maybe String  }

-- | The set of slots for the root container
type ChildSlots = ( menu ∷ Menu.Slot Unit,
                    alert ∷ Alert.Slot Unit,
                    footer ∷ Footer.Slot Unit,
                    login ∷ Login.Slot Unit)
                  
_menu = SProxy::SProxy "menu"
_alert = SProxy::SProxy "alert"
_footer = SProxy::SProxy "footer"
_login = SProxy::SProxy "login"

-- | The root component definition
component :: ∀ q i o r m .
             MonadAff m
             ⇒ MonadAsk { userName :: String | r } m
             ⇒ H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

-- | Initial state, for instance a not logged in user
initialState ∷ ∀ i. i → State
initialState _ = { user: Nothing }

-- | Render the root application
render ∷ ∀ a m. State → H.ComponentHTML a ChildSlots m
render state = HH.div
               [css "container"]
               [HH.slot _menu unit Menu.component unit absurd,
                HH.div
                [css "row"]
                [HH.div
                 [css "col-md-12"]
                 [HH.slot _alert unit Alert.component unit absurd]
                ],
                HH.main
                [ ]
                [HH.slot _login unit Login.component unit absurd],
                HH.div
                []
                [HH.slot _footer unit Footer.component unit absurd]
               ]

handleAction ∷ ∀ r o m .
                MonadAff m ⇒
                MonadAsk { userName :: String | r } m ⇒
                Action → H.HalogenM State Action ChildSlots o m Unit
handleAction SetUser =
  do
    H.liftEffect $ log "Hejsan"
    name <- lift $ asks _.userName
    state <- H.get
    H.put $ state { user = Just name }
