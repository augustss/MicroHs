module Data.Records(module Data.Records) where
import Primitives
import Data.Function
import Data.Proxy

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
