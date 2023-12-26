module Data.Record(module Data.Record) where
import Primitives
import Data.Function
import Data.Proxy

type  HasField :: forall (k::Kind) . k -> Type -> Type -> Constraint
class HasField x r a | x r -> a where
  hasField :: Proxy x -> r -> (a, a -> r)

getField :: forall x r a . HasField x r a => Proxy x -> r -> a
getField p r = case hasField p r of { (g,_) -> g }

setField :: forall x r a . HasField x r a => Proxy x -> r -> (a -> r)
setField p r = case hasField p r of { (_,s) -> s }
