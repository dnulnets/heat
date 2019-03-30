-- |
-- | The root page for the entire application.
-- |
-- | Written by Tomas Stenlund, Sundsvall, Sweden (c) 2019
-- |
module Slip.Component.HTML.Utils where

-- | Language imports
import Prelude
import Data.Maybe (Maybe(..))

-- | Halogen imports
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (AttrName(..), PropName(..))

-- | Helper function for adding class to HTML tags
css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

-- | A helper for the style property
style :: forall r i. String -> HH.IProp (style :: String | r) i
style = HP.prop (PropName "style")

-- | A generic property string function
prop :: forall r i. String -> String -> HP.IProp r i
prop name = HP.attr (AttrName name)

-- | Sometimes we need to deal with elements which may or may not exist. This function lets us
-- | provide rendering for the element if it exists, and renders an empty node otherwise.
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | PureScript is a strict language. If we want to conditionally display an element, then we
-- | should hide the evaluation behind a function, which won't be evaluated right away, in order
-- | to minimize the work performed each render.
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
