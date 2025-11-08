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

gcast1 :: forall c t t' a . (Typeable t, Typeable t') => c (t a) -> Maybe (c (t' a))

gcast2 :: forall c t t' a b . (Typeable t, Typeable t') => c (t a b) -> Maybe (c (t' a b))
