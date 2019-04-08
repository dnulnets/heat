-- |
-- | The root page for the entire application.
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Root (component,
                  Query (..)) where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Control.Monad.Reader.Trans (class MonadAsk, asks)
import Control.Monad.Trans.Class (lift)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- | Halogen imports
import Halogen as H
import Halogen.HTML as HH

-- | Slip imports
import Slip.Child as CH
import Slip.Data.Route (Page(..))
import Slip.Data.Alert as AL
import Slip.Component.HTML.Utils (css, style)
import Slip.Component.Menu as Menu
import Slip.Component.Alert as Alert
import Slip.Component.Footer as Footer
import Slip.Component.Login as Login
import Slip.Component.Home as Home

-- | The querys supported by the root page
data Query a = GotoPage Page a

-- | The actions supported by the root page
data Action = SetUser |
              LoginMessage CH.Message |
              HomeMessage CH.Message

-- | The state for the application, it will contain the logged in user among other things
type State = { user ∷ Maybe String,
               page ∷ Page,
               alert ∷ Maybe AL.Alert }

-- | The set of slots for the root container
type ChildSlots = ( menu ∷ Menu.Slot Unit,
                    alert ∷ Alert.Slot Unit,
                    footer ∷ Footer.Slot Unit,
                    main ∷ CH.Slot String)
                  
_menu = SProxy::SProxy "menu"
_alert = SProxy::SProxy "alert"
_footer = SProxy::SProxy "footer"
_main = SProxy::SProxy "main"

-- | The root component definition
component :: ∀ i o r m .
             MonadAff m
             ⇒ MonadAsk { userName :: String | r } m
             ⇒ H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction,
                                       handleQuery = handleQuery }
    }

-- | Initial state, for instance a not logged in user
initialState ∷ ∀ i. i → State
initialState _ = { user: Nothing,
                   page: Home,
                   alert: Nothing }

-- | Render the root application, it contains a menu, alert data, main page view and a footer
render ∷ ∀ m . MonadAff m ⇒ State → H.ComponentHTML Action ChildSlots m
render state = HH.div
               [css "container"]
               [HH.slot _menu unit Menu.component unit absurd,
                HH.div
                [css "row"]
                [HH.div
                 [css "col-md-12"]
                 [HH.slot _alert unit Alert.component state.alert absurd]
                ],
                HH.main
                [ ]
                [view state.page],
                HH.div
                []
                [HH.slot _footer unit Footer.component unit absurd]
               ]

-- | Render the main view of the page
view ∷ ∀ m. MonadAff m ⇒ Page → H.ComponentHTML Action ChildSlots m
view Login = HH.slot _main "login" Login.component unit (Just <<< LoginMessage)
view Home = HH.slot _main "home" Home.component unit (Just <<< HomeMessage)
view _ = HH.div
             [css "container", style "margin-top:20px"]
             [HH.div
              [css "row"]
              [HH.div
               [css "col-md-12"]
               [HH.div
                [css "col-md-3 col-md-offset-1"]
                [HH.h2
                 []
                 [HH.text "ERROR Unknown page"]
                ]
               ]
              ]
             ]

-- | Handle the route change from the browser
handleQuery ∷ ∀ r o m a .
              MonadAff m ⇒ 
              MonadAsk { userName :: String | r } m ⇒ 
              Query a → H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  GotoPage page a → do
    state ← H.get
    H.put $ state { page = page, alert = Nothing }
    pure (Just a)

-- | Just a placeholder for how to write a handler for actions
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
    
handleAction ( LoginMessage (CH.GotoPage url)) =
  do
    H.liftEffect $ log "Go to a new page"
    state <- H.get
    H.put $ state { page = url, alert = Nothing }

handleAction ( LoginMessage (CH.Alert alrt msg)) =
  do
    H.liftEffect $ log "Alerting the user ..."
    state <- H.get
    H.put $ state { alert = Just $ AL.Alert alrt msg }
      
handleAction _ =
  do
    H.liftEffect $ log "Unhandled action"
    


--
-- Scratch area
--
-- How to send a request
-- void $ H.query _alert unit (H.tell (Alert.Alert alrt msg))
