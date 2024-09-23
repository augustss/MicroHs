module Data.Foldable.Internal(module Data.Foldable.Internal) where
import Prelude()
import Data.Bool
import Data.Maybe_Type
import Data.Monoid.Internal hiding (Max(..), Min(..))
import Data.List
import Data.Ord

newtype Max a = Max (Maybe a)
getMax :: forall a . Max a -> Maybe a
getMax (Max ma) = ma

newtype Min a = Min (Maybe a)
getMin :: forall a . Min a -> Maybe a
getMin (Min ma) = ma

instance forall a . Ord a => Semigroup (Max a) where
    m <> Max Nothing = m
    Max Nothing <> n = n
    (Max m@(Just x)) <> (Max n@(Just y))
      | x >= y    = Max m
      | otherwise = Max n

instance forall a . Ord a => Monoid (Max a) where
    mempty = Max Nothing
    mconcat = foldl' (<>) mempty

instance forall a . Ord a => Semigroup (Min a) where
    m <> Min Nothing = m
    Min Nothing <> n = n
    (Min m@(Just x)) <> (Min n@(Just y))
      | x <= y    = Min m
      | otherwise = Min n

instance forall a . Ord a => Monoid (Min a) where
    mempty = Min Nothing
    mconcat = foldl' (<>) mempty
