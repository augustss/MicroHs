module Data.Monoid(
  Monoid(..),
  Endo(..),
  Dual(..),
  Sum(..),
  Product(..),
  All(..),
  Any(..),
  Arg(..),
  Alt(..),
  First(..),
  Last(..),
  ) where
import Prelude()
import Data.Maybe_Type
import Data.Monoid.Internal
import Data.Records

-- First and Last are different in Monoid and Semigroup,
-- so put them here.

newtype First a = First { getFirst :: Maybe a }

instance forall a . Semigroup (First a) where
  a@(First (Just _)) <> _ = a
  First Nothing      <> a = a

instance forall a . Monoid (First a) where
  mempty = First Nothing


newtype Last a = Last { getLast :: Maybe a }

instance forall a . Semigroup (Last a) where
  _ <> a@(Last (Just _)) = a
  a <>    Last Nothing   = a

instance forall a . Monoid (Last a) where
  mempty = Last Nothing
