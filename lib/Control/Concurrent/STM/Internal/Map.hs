module Control.Concurrent.STM.Internal.Map(
  Map,
  empty,
  null,
  lookup,
  insert,
  delete,
  ) where
import Prelude hiding(lookup, null)
import qualified Prelude as P

newtype Map k v = Map [(k, v)]

empty :: Map k v
empty = Map []

null :: Map k v -> Bool
null (Map m) = P.null m

lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map m) = P.lookup k m

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map m) = Map ((k, v):m)

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map m) = Map $ filter ((k /=) . fst) m
