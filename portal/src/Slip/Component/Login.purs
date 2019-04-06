-- |
-- | The login component
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.Login where

-- | Language imports
import Prelude

-- | Halogen import
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | DOM import
import DOM.HTML.Indexed.InputType (InputType(..))

-- | Our own stuff
import Slip.Component.HTML.Utils (css, style)

-- | State for the component
type State = { }

-- | Initial state is no logged in user
initialState ∷ ∀ i. i → State
initialState _ = { }

-- | The component definition
component ∷ ∀ q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

-- | Render the alert
render ∷ ∀ a m. State → H.ComponentHTML a () m
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
                    [HP.name "form"]
                    [HH.div
                     [css "form-group"]
                     [HH.label [HP.for "username"][HH.text "Username"],
                      HH.input [css "form-control", HP.name "username", HP.type_ InputText, HP.required true]
                     ],
                     HH.div
                     [css "form-group"]
                     [HH.label [HP.for "password"][HH.text "Password"],
                      HH.input [css "form-control", HP.name "password", HP.type_ InputPassword, HP.required true]
                     ],HH.div
                     [css "form-group"]
                     [HH.button [css "btn btn-primary"][HH.text "Login"]
                     ]
                    ]
                  ]
                 ]
                ]
               ]
