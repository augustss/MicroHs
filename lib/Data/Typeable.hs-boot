module Data.Typeable where
import qualified Prelude()
import Data.Char_Type
import Data.Maybe_Type

type  Typeable :: forall k . k -> Constraint
class Typeable a where
  typeRep :: forall proxy . proxy a -> TypeRep

data TypeRep

_mkTyCon :: forall a . String -> String -> a -> TypeRep

cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
