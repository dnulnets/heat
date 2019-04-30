-- |
-- | The HTML Utilities module
-- |
module Heat.Component.HTML.Utils where

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

-- | Render a fragment if the the value exists (Just), empty if not (Nothing)
maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x
maybeElem _ _ = HH.text ""

-- | Render a fragment if the value is true, empty if not
whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""
