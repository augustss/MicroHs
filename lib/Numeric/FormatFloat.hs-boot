module Numeric.FormatFloat where
import qualified Prelude()
import Data.Char_Type
import Data.RealFloat
--import Data.Text

showFloat :: (RealFloat a) => a -> {-ShowS-} (String -> String)
