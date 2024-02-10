module Data.Records(
  module Data.Proxy,
  HasField(..),
  SetField(..),
  hasField,
  composeSet,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Function
import Data.Proxy
import Data.Tuple

type Get r a = r -> a
type Set r a = r -> a -> r
type GetSet r a = r -> (a, a -> r)

type  HasField :: forall (k::Kind) . k -> Type -> Type -> Constraint
class HasField x r a | x r -> a where
  getField :: Proxy x -> r -> a -- Get r a

type  SetField :: forall (k::Kind) . k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  setField :: Proxy x -> r -> a -> r -- Set r a

hasField :: forall x r a . (HasField x r a, SetField x r a) => Proxy x -> r -> (a, a -> r)                    -- GetSet r a
hasField p r = (getField p r, setField p r)

composeSet :: forall a b c . GetSet a b -> (b -> c -> b) -> (a -> c -> a)
composeSet gs1 b_to_c_to_b a c =
  case gs1 a of
    (b, b_to_a) -> b_to_a (b_to_c_to_b b c)

-----------------------------------
-- Virtual fields for tuples.

instance forall a b . HasField "_1" (a, b) a where getField _ (a, b) = a
instance forall a b . SetField "_1" (a, b) a where setField _ (a, b) = \ a -> (a, b)
instance forall a b . HasField "_2" (a, b) b where getField _ (a, b) = b
instance forall a b . SetField "_2" (a, b) b where setField _ (a, b) = \ b -> (a, b)

instance forall a b c . HasField "_1" (a, b, c) a where getField _ (a, b, c) = a
instance forall a b c . SetField "_1" (a, b, c) a where setField _ (a, b, c) = \ a -> (a, b, c)
instance forall a b c . HasField "_2" (a, b, c) b where getField _ (a, b, c) = b
instance forall a b c . SetField "_2" (a, b, c) b where setField _ (a, b, c) = \ b -> (a, b, c)
instance forall a b c . HasField "_3" (a, b, c) c where getField _ (a, b, c) = c
instance forall a b c . SetField "_3" (a, b, c) c where setField _ (a, b, c) = \ c -> (a, b, c)

instance forall a b c d . HasField "_1" (a, b, c, d) a where getField _ (a, b, c, d) = a
instance forall a b c d . SetField "_1" (a, b, c, d) a where setField _ (a, b, c, d) = \ a -> (a, b, c, d)
instance forall a b c d . HasField "_2" (a, b, c, d) b where getField _ (a, b, c, d) = b
instance forall a b c d . SetField "_2" (a, b, c, d) b where setField _ (a, b, c, d) = \ b -> (a, b, c, d)
instance forall a b c d . HasField "_3" (a, b, c, d) c where getField _ (a, b, c, d) = c
instance forall a b c d . SetField "_3" (a, b, c, d) c where setField _ (a, b, c, d) = \ c -> (a, b, c, d)
instance forall a b c d . HasField "_4" (a, b, c, d) d where getField _ (a, b, c, d) = d
instance forall a b c d . SetField "_4" (a, b, c, d) d where setField _ (a, b, c, d) = \ d -> (a, b, c, d)
