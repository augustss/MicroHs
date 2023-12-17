module Data.Monoid(module Data.Monoid, module Data.Semigroup) where
import Primitives
import Control.Applicative
import Control.Monad
import Data.Bounded
import Data.Function
import Data.Functor
import Data.List_Type
import Data.Ord
import Data.Semigroup

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
  mconcat :: [a] -> a
  mconcat [] = mempty
  mconcat (a:as) = a <> mconcat as

---------------------

newtype Endo a = Endo (a -> a)
appEndo :: forall a . Endo a -> (a -> a)
appEndo (Endo f) = f

instance forall a . Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance forall a . Monoid (Endo a) where
  mempty = Endo id

---------------------

newtype Dual a = Dual a
getDual :: forall a . Dual a -> a
getDual (Dual a) = a

instance forall a . Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance forall a . Monoid a => Monoid (Dual a) where
  mempty = Dual mempty

instance Functor Dual where
  fmap f (Dual a) = Dual (f a)

instance Applicative Dual where
  pure = Dual
  Dual f <*> Dual b = Dual (f b)

instance Monad Dual where
  m >>= k = k (getDual m)

---------------------

newtype Max a = Max a
getMax :: forall a . Max a -> a
getMax (Max a) = a

instance forall a . Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (a `max` b)

instance forall a . (Ord a, Bounded a) => Monoid (Max a) where
  mempty = Max minBound

---------------------

newtype Min a = Min a
getMin :: forall a . Min a -> a
getMin (Min a) = a

instance forall a . Ord a => Semigroup (Min a) where
  Min a <> Min b = Min (a `min` b)

instance forall a . (Ord a, Bounded a) => Monoid (Min a) where
  mempty = Min maxBound
