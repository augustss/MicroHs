module Data.Ord(
  module Data.Ord,
  module Data.Ordering_Type,
  ) where
import Primitives
import Data.Bool_Type
import Data.Ordering_Type
import Data.Eq

infix 4 <,<=,>,>=

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  -- XXX Check with the Haskell report
  compare x y = if x <= y then (if y <= x then EQ else LT) else GT
  x < y   = if y <= x then False else True
  x > y   = if x <= y then False else True
  x >= y  = x <= y
  min x y = if x <= y then x else y
  max x y = if x <= y then y else x

instance Eq Ordering where
  LT == LT  =  True
  EQ == EQ  =  True
  GT == GT  =  True
  _  == _   =  False

isEQ :: Ordering -> Bool
isEQ EQ = True
isEQ _  = False

