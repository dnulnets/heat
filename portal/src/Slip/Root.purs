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

-- | Halogen imports
import Halogen as H
import Halogen.HTML as HH

-- | Slip imports
import Slip.Component.HTML.Utils (css, style)
import Slip.Component.Menu as Menu
import Slip.Component.Alert as Alert
import Slip.Component.Footer as Footer

-- | The state for the application, it will contain the logged in user among other things
type State = { user :: Maybe String  }

-- | The set of slots for the root container
type ChildSlots = ( menu :: Menu.Slot Unit,
                    alert :: Alert.Slot Unit,
                    footer :: Footer.Slot Unit)
                  
_menu = SProxy::SProxy "menu"
_alert = SProxy::SProxy "alert"
_footer = SProxy::SProxy "footer"

-- | The root component definition
component :: forall f i o m. H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

-- | Initial state, for instance a not logged in user
initialState :: forall i. i -> State
initialState _ = { user: Nothing }

-- | Render the root application
render :: forall a m. State -> H.ComponentHTML a ChildSlots m
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
                [HH.div
                 [css "container", style "margin-top:20px"]
                 [HH.div
                  [css "row"]
                  [HH.div
                   [css "col-md-12"]
                   [HH.div
                    [css "col-md-3 col-md-offset-1"]
                    [HH.h2
                     []
                     [HH.text "Router page"]
                    ]
                   ]
                  ]
                 ]
                ],
                HH.div
                []
                [HH.slot _footer unit Footer.component unit absurd]
               ]
               
               
