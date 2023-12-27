module Data.Records(module Data.Records) where
import Primitives
import Data.Function
import Data.Proxy
import Data.Tuple

type GetSet r a = r -> (a, a -> r)

type  HasField :: forall (k::Kind) . k -> Type -> Type -> Constraint
class HasField x r a | x r -> a where
  hasField :: Proxy x -> GetSet r a

getField :: forall x r a . HasField x r a => Proxy x -> r -> a
getField p r = case hasField p r of { (g,_) -> g }

setField :: forall x r a . HasField x r a => Proxy x -> r -> (a -> r)
setField p r = case hasField p r of { (_,s) -> s }

composeGetSet :: forall a b c . GetSet a b -> GetSet b c -> GetSet a c
composeGetSet gs1 gs2 a =
  case gs1 a of
    (b, b_to_a) ->
      case gs2 b of
        (c, c_to_b) -> (c, b_to_a . c_to_b)

composeSet :: forall a b c . GetSet a b -> (b -> c -> b) -> (a -> c -> a)
composeSet gs1 b_to_c_to_b a c =
  case gs1 a of
    (b, b_to_a) -> b_to_a (b_to_c_to_b b c)

-----------------------------------
-- Virtual fields for tuples.

instance forall a b . HasField "_1" (a, b) a where
  hasField _ (a, b) = (a, \ a -> (a, b))
instance forall a b . HasField "_2" (a, b) b where
  hasField _ (a, b) = (b, \ b -> (a, b))

instance forall a b c . HasField "_1" (a, b, c) a where
  hasField _ (a, b, c) = (a, \ a -> (a, b, c))
instance forall a b c . HasField "_2" (a, b, c) b where
  hasField _ (a, b, c) = (b, \ b -> (a, b, c))
instance forall a b c . HasField "_3" (a, b, c) c where
  hasField _ (a, b, c) = (c, \ c -> (a, b, c))

instance forall a b c d . HasField "_1" (a, b, c, d) a where
  hasField _ (a, b, c, d) = (a, \ a -> (a, b, c, d))
instance forall a b c d . HasField "_2" (a, b, c, d) b where
  hasField _ (a, b, c, d) = (b, \ b -> (a, b, c, d))
instance forall a b c d . HasField "_3" (a, b, c, d) c where
  hasField _ (a, b, c, d) = (c, \ c -> (a, b, c, d))
instance forall a b c d . HasField "_4" (a, b, c, d) d where
  hasField _ (a, b, c, d) = (d, \ d -> (a, b, c, d))
