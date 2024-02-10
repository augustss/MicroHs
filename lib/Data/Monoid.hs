module Data.Monoid(module Data.Monoid, module Data.Semigroup) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Applicative
import Data.Bool
import Data.Bounded
import Data.Function
import Data.Functor
import Data.List_Type
import Data.Ord
import Data.Maybe_Type
import Data.Num
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

---------------------

newtype Sum a = Sum a
getSum :: forall a . Sum a -> a
getSum (Sum a) = a

instance forall a . Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance forall a . (Num a) => Monoid (Sum a) where
  mempty = Sum 0

---------------------

newtype Product a = Product a
getProduct :: forall a . Product a -> a
getProduct (Product a) = a

instance forall a . Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance forall a . (Num a) => Monoid (Product a) where
  mempty = Product 1

---------------------

newtype All = All Bool
getAll :: All -> Bool
getAll (All a) = a

instance Semigroup All where
  All a <> All b = All (a && b)

instance Monoid All where
  mempty = All True

---------------------

newtype Any = Any Bool
getAny :: Any -> Bool
getAny (Any a) = a

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Monoid Any where
  mempty = Any False

---------------------

newtype First a = First (Maybe a)
getFirst :: forall a . First a -> Maybe a
getFirst (First a) = a

instance forall a . Semigroup (First a) where
  a@(First (Just _)) <> _ = a
  First Nothing      <> a = a

instance forall a . Monoid (First a) where
  mempty = First Nothing

---------------------

newtype Last a = Last (Maybe a)
getLast :: forall a . Last a -> Maybe a
getLast (Last a) = a

instance forall a . Semigroup (Last a) where
  _ <> a@(Last (Just _)) = a
  a <>    Last Nothing   = a

instance forall a . Monoid (Last a) where
  mempty = Last Nothing

---------------------

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> o = o
  GT <> _ = GT

instance Monoid Ordering where
  mempty = EQ
