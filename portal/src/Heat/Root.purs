-- |
-- | The root page for the entire application.
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Heat.Root (component,
                  Query (..)) where

-- Language imports
import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

import Control.Monad.Reader.Trans (class MonadAsk)

import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Console (log)

-- Halogen imports
import Halogen as H
import Halogen.HTML as HH

-- Heat imports
import Heat.Data.Route (Page(..))
import Heat.Data.Alert as HDAL
import Heat.Component.HTML.Utils (css, style, prop)
import Heat.Component.Menu as Menu
import Heat.Component.Alert as Alert
import Heat.Component.Footer as Footer
import Heat.Component.Login as Login
import Heat.Component.Home as Home
import Heat.Component.User as User
import Heat.Interface.Authenticate (UserInfo(..),
                                    class ManageAuthentication)
import Heat.Interface.Navigate (class ManageNavigation)
import Heat.Interface.User (class ManageUsers)

-- | The querys supported by the root page
data Query a = GotoPageRequest Page a -- ^Goto page query from the parent

-- | The actions supported by the root page
data Action = AlertAction    HDAL.Alert         -- ^Alert action from the children
            | SetUserAction  (Maybe UserInfo)   -- ^Sets the user
            | LogoutAction                      -- ^Logs out the user
            | ReloadAction                      -- ^Reloads the current page
              
-- | The state for the application, it will contain the logged in user among other things
type State = { user ∷ Maybe UserInfo,
               page ∷ Page,
               alert ∷ Maybe HDAL.Alert,
               shown ∷ Boolean}

-- | The set of slots for the root container
type ChildSlots = ( menu ∷ Menu.Slot Unit,
                    alert ∷ Alert.Slot Unit,
                    footer ∷ Footer.Slot Unit,
                    login ∷ Login.Slot Unit,
                    home ∷ Home.Slot Unit,
                    user ∷ User.Slot Unit)
                  
_menu = SProxy::SProxy "menu"
_alert = SProxy::SProxy "alert"
_footer = SProxy::SProxy "footer"
_login = SProxy::SProxy "login"
_home = SProxy::SProxy "home"
_user = SProxy::SProxy "user"

-- | The root component definition
component :: ∀ i o r m . MonadAff m
             ⇒ ManageUsers m
             ⇒ ManageAuthentication m
             ⇒ ManageNavigation m
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
                   alert: Nothing,
                   shown: true}

-- | Render the root application, it contains a menu, alert data, main page view and a footer
render ∷ ∀ m r. MonadAff m
         ⇒ ManageUsers m
         ⇒ ManageAuthentication m
         ⇒ ManageNavigation m
         ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
         ⇒ State → H.ComponentHTML Action ChildSlots m
render state = HH.div
               []
               [HH.slot _menu unit Menu.component (userConv <$> state.user) (Just <<< menuMessageConv),
                HH.div
                [css "container"]
                [HH.div
                 [css "col-md-12"]
                 [HH.slot _alert unit Alert.component state.alert absurd]
                ],
                HH.main
                [css "container", prop "role" "main"]
                [view state.page],
                HH.div
                []
                [HH.slot _footer unit Footer.component unit absurd]
               ]
  where
    userConv∷UserInfo->Menu.UserInfo
    userConv (UserInfo u) = Menu.UserInfo { userid : u.userid, 
                                            username : u.username,
                                            role : u.role,
                                            level : u.level }

-- | Render the main view of the page
view ∷ ∀ r m. MonadAff m
       ⇒ ManageAuthentication m
       ⇒ ManageUsers m
       ⇒ ManageNavigation m
       ⇒ MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m
       ⇒ Page → H.ComponentHTML Action ChildSlots m
view Login = HH.slot _login unit Login.component unit (Just <<< loginMessageConv)
view Home = HH.slot _home unit Home.component unit absurd
view (User userid) = HH.slot _user unit User.component userid absurd
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

-- |Converts menu messages to root actions
menuMessageConv::Menu.Message->Action
menuMessageConv Menu.LogoutMessage = LogoutAction
menuMessageConv (Menu.AlertMessage alert) = AlertAction alert 

-- |Converts login messages to root actions
loginMessageConv::Login.Message->Action
loginMessageConv (Login.SetUserMessage user) = SetUserAction user
loginMessageConv (Login.AlertMessage alert) = AlertAction alert 
loginMessageConv Login.ReloadMessage = ReloadAction

-- | Handle the queries sent to the root page
handleQuery ∷ ∀ r o m a .
              MonadAff m ⇒ 
              MonadAsk { userInfo ∷ Ref (Maybe UserInfo) | r } m ⇒ 
              Query a → H.HalogenM State Action ChildSlots o m (Maybe a)
handleQuery = case _ of
  GotoPageRequest newpage a → do
    state ← H.get
    H.liftEffect $ log $ "GotoPageRequest from " <> show state.page <> " to " <> show newpage
    H.put $ state { page = newpage, shown = true, alert = if state.shown then Nothing else state.alert }
    pure (Just a)

-- | Handle the actions within the root page
handleAction ∷ ∀ r o m .
                MonadAff m ⇒
                MonadAsk { userInfo :: Ref (Maybe UserInfo) | r } m ⇒
                Action → H.HalogenM State Action ChildSlots o m Unit

-- | Reload => Reloads a page
handleAction ReloadAction = do
  state <- H.get
  H.liftEffect $ log "Reload page"
  H.put $ state { shown = true, alert = if state.shown then Nothing else state.alert }

-- | Alert level message  => Sets the alert message and level for the root page, to be renderd by the
-- | alert view.
handleAction (AlertAction alert) = do
  state <- H.get
  H.put $ state { alert = Just $ alert, shown = false }

-- | SetUser userinfo => Sets the logged in user
handleAction (SetUserAction ui) = do
  state <- H.get
  H.put $ state { user = ui }

-- | Logout => Logs out the user
handleAction LogoutAction = do
  state <- H.get
  H.put $ state { user = Nothing }
  
