module Data.Monoid(
  Monoid(..),
  Endo(..), appEndo,
  Dual(..), getDual,
  Max(..), getMax,
  Min(..), getMin,
  Sum(..), getSum,
  Product(..), getProduct,
  All(..), getAll,
  Any(..), getAny,
  Arg(..), ArgMin, ArgMax,
  Alt(..), getAlt,
  First(..), getFirst,
  Last(..), getLast,
  ) where
import Prelude()
import Data.Maybe_Type
import Data.Monoid.Internal

-- First and Last are different in Monoid and Semigroup,
-- so put them here.

newtype First a = First (Maybe a)
getFirst :: forall a . First a -> Maybe a
getFirst (First a) = a

instance forall a . Semigroup (First a) where
  a@(First (Just _)) <> _ = a
  First Nothing      <> a = a

instance forall a . Monoid (First a) where
  mempty = First Nothing


newtype Last a = Last (Maybe a)
getLast :: forall a . Last a -> Maybe a
getLast (Last a) = a

instance forall a . Semigroup (Last a) where
  _ <> a@(Last (Just _)) = a
  a <>    Last Nothing   = a

instance forall a . Monoid (Last a) where
  mempty = Last Nothing
