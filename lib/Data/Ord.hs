module Data.Ord(
  module Data.Ord,
  module Data.Ordering_Type,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Data.Bool_Type
import Data.Bounded
import Data.Eq
import Data.Functor
import Data.Ordering_Type
import Data.Records
import {-# SOURCE #-} Data.Typeable
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

deriving instance Eq Ordering
deriving instance Ord Ordering
deriving instance Show Ordering
deriving instance Bounded Ordering

comparing :: (Ord b) => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)

clamp :: (Ord a) => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)

newtype Down a = Down
    { getDown :: a -- ^ @since 4.14.0.0
    }
{- In Data.Orphans
      ( Num
      , Semigroup
      , Monoid
      , Bits
      , FiniteBits
      , Floating
      , Fractional
      , Ix
      , Real
      , RealFrac
      , RealFloat
      , Storable
      , Read
      , Show
      , Functor
      )
-}

instance Eq a => Eq (Down a) where
  Down x == Down y  =  x == y

instance Ord a => Ord (Down a) where
  compare (Down x) (Down y) = y `compare` x

instance Bounded a => Bounded (Down a) where
  minBound = Down maxBound
  maxBound = Down minBound

instance Functor Down where
  fmap f (Down a) = Down (f a)
