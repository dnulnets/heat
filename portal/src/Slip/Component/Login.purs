-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Login where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style)
import Slip.Data.Route (Page(..))

-- | State for the component
type State = { username∷Maybe String,
               password∷Maybe String}

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { username : Nothing,
                   password : Nothing }

-- | Messages sent out from the component
data Output = GotoPage Page

-- | Internal form actions
data Action = Submit
            | Input (State->State)

-- | The component definition
component ∷ ∀ q i o m .
            MonadAff m ⇒
            H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction}
    }

-- | Render the alert
render ∷ ∀ m . State → H.ComponentHTML Action () m
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
                   [HH.text "Login"],
                   HH.form
                   []
                   [HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "username"][HH.text "Username"],
                     HH.input [css "form-control", HP.name "username", HP.type_ HP.InputText,
                               HE.onValueChange \v -> Just $ Input (\st -> st { username = Just v })]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.label [HP.for "password"][HH.text "Password"],
                     HH.input [css "form-control", HP.name "password", HP.type_ HP.InputPassword,
                               HE.onValueChange \v -> Just $ Input (\st  -> st { password = Just v })]
                    ],
                    HH.fieldset
                    [css "form-group"]
                    [HH.button [css "btn btn-primary", HP.type_ HP.ButtonButton,
                                HE.onClick \e -> Just $ Submit] [HH.text "Login"]
                    ]
                   ]
                  ]
                 ]
                ]
               ]

handleAction ∷ ∀ o m .
               MonadAff m ⇒
               Action → H.HalogenM State Action () o m Unit
handleAction Submit = do
  state <- H.get
  H.liftEffect $ log $ "Submit" <> fromMaybe "<nothing>" state.username <> " " <> fromMaybe "<nothing>" state.password

handleAction (Input f) = do
  H.liftEffect $ log $ "Setinput"
  state <- H.get
  H.put $ f state
