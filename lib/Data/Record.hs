module Data.Record(module Data.Record) where
import Primitives
import Data.Function
import Data.Proxy
import Data.Tuple

type  HasField :: forall (k::Kind) . k -> Type -> Type -> Constraint
class HasField x r a | x r -> a where
  hasField :: Proxy x -> r -> (a, a -> r)

getField :: forall x r a . HasField x r a => Proxy x -> r -> a
getField p = fst . hasField p

setField :: forall x r a . HasField x r a => Proxy x -> r -> a -> r
setField p = snd . hasField p
