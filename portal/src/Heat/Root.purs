-- |
-- | The root page for the entire application.
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Root (component,
                  Query (..)) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)

-- Halogen imports
import Halogen as H
import Halogen.HTML as HH

-- Heat imports
import Heat.Data.Route (Page(..))
import Heat.Data.Alert as HDAL
import Heat.Component.HTML.Utils (css, style)
import Heat.Component.Menu as Menu
import Heat.Component.Alert as Alert
import Heat.Component.Footer as Footer
import Heat.Component.Login as Login
import Heat.Component.Home as Home
import Heat.Interface.Authenticate (UserInfo(..),
                                    class ManageAuthentication)

-- | The querys supported by the root page
data Query a = GotoPage Page a

-- | The actions supported by the root page
data Action = LoginMessage Login.Message
              
-- | The state for the application, it will contain the logged in user among other things
type State = { user ∷ Maybe UserInfo,
               page ∷ Page,
               alert ∷ Maybe HDAL.Alert }

-- | The set of slots for the root container
type ChildSlots = ( menu ∷ Menu.Slot Unit,
                    alert ∷ Alert.Slot Unit,
                    footer ∷ Footer.Slot Unit,
                    login ∷ Login.Slot Unit,
                    home ∷ Home.Slot Unit)
                  
_menu = SProxy::SProxy "menu"
_alert = SProxy::SProxy "alert"
_footer = SProxy::SProxy "footer"
_login = SProxy::SProxy "login"
_home = SProxy::SProxy "home"

-- | The root component definition
component :: ∀ i o r m . MonadAff m
             ⇒ ManageAuthentication m
             ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
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
render ∷ ∀ m r. MonadAff m
         ⇒ ManageAuthentication m
         ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
         ⇒ State → H.ComponentHTML Action ChildSlots m
render state = HH.div
               [css "container"]
               [HH.slot _menu unit Menu.component (userConv <$> state.user) absurd,
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

userConv∷UserInfo->Menu.UserInfo
userConv (UserInfo u) = Menu.UserInfo { username : u.username, role : u.role, level : u.level }

-- | Render the main view of the page
view ∷ ∀ r m. MonadAff m
       ⇒ ManageAuthentication m
       ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
       ⇒ Page → H.ComponentHTML Action ChildSlots m
view Login = HH.slot _login unit Login.component unit (Just <<< LoginMessage)
view Home = HH.slot _home unit Home.component unit absurd
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

-- | Handle the queries sent to the root page
handleQuery ∷ ∀ r o m a .
              MonadAff m ⇒ 
              MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m ⇒ 
              Query a → H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  GotoPage page a → do
    state ← H.get
    H.put $ state { page = page, alert = Nothing }
    pure (Just a)

-- | Handle the actions within the root page
handleAction ∷ ∀ r o m .
                MonadAff m ⇒
                MonadAsk { userInfo :: Ref (Maybe UserInfo) | r } m ⇒
                Action → H.HalogenM State Action ChildSlots o m Unit

-- | Handles messages sent out from the Login view
-- |
-- | Alert level message  => Sets the alert message and level for the root page, to be renderd by the
-- | alert view.
handleAction ( LoginMessage (Login.Alert alrt msg)) =
  do
    state <- H.get
    H.put $ state { alert = Just $ HDAL.Alert alrt msg }

-- | SetUser userinfo => Sets the logged in user and redirect to landing page
handleAction ( LoginMessage (Login.SetUser ui)) =
  do
    state <- H.get
    H.put $ state { user = ui, page = maybe Login (\_->Home) ui }

