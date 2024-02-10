module Data.Ord(
  module Data.Ord,
  module Data.Ordering_Type,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool_Type
import Data.Bounded
import Data.Ordering_Type
import Data.Eq
import Text.Show

infix 4 <,<=,>,>=

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

  compare x y = if x == y then EQ
                else if x <= y then LT
                else GT

  x <= y = case compare x y of { GT -> False; _ -> True }
  x >= y = y <= x
  x > y = if x <= y then False else True
  x < y = if y <= x then False else True

  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

instance Eq Ordering where
  LT == LT  =  True
  EQ == EQ  =  True
  GT == GT  =  True
  _  == _   =  False

instance Show Ordering where
  showsPrec _ LT = showString "LT"
  showsPrec _ EQ = showString "EQ"
  showsPrec _ GT = showString "GT"

instance Bounded Ordering where
  minBound = LT
  maxBound = GT
