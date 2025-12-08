module Data.Monoid(
  Monoid(..),
  Endo(..),
  Dual(..),
  Sum(..),
  Product(..),
  All(..),
  Any(..),
  Alt(..),
  First(..),
  Last(..),
  ) where
import qualified Prelude()
import Data.Maybe_Type
import Data.Monoid.Internal
import Data.Records
import {-# SOURCE #-} Data.Typeable

-- First and Last are different in Monoid and Semigroup,
-- so put them here.

newtype First a = First { getFirst :: Maybe a }

instance Semigroup (First a) where
  a@(First (Just _)) <> _ = a
  First Nothing      <> a = a

instance Monoid (First a) where
  mempty = First Nothing


newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
  _ <> a@(Last (Just _)) = a
  a <>    Last Nothing   = a

instance Monoid (Last a) where
  mempty = Last Nothing
