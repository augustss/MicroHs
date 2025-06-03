module Control.Concurrent.STM.Internal.Set(
  Set,
  empty,
  member,
  insert,
  delete,
  elems,
  union,
  (\\),
  ) where
import Prelude hiding(union)
import qualified Data.List

newtype Set a = Set [a]

empty :: Set a
empty = Set []

member :: Ord a => a -> Set a -> Bool
member a (Set s) = elem a s

insert :: Ord a => a -> Set a -> Set a
insert a s = union (Set [a]) s

delete :: Ord a => a -> Set a -> Set a
delete a (Set s) = Set (filter (a /=) s)

elems :: Ord a => Set a -> [a]
elems (Set s) = Data.List.sort s

union :: Ord a => Set a -> Set a -> Set a
union (Set s1) (Set s2) = Set (Data.List.union s1 s2)

(\\) :: Ord a => Set a -> Set a -> Set a
Set s1 \\ Set s2 = Set (s1 Data.List.\\ s2)
