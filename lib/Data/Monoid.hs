module Data.Monoid(module Data.Monoid) where
import Primitives
import Data.List_Type
import Data.Semigroup

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
  mconcat :: [a] -> a
  mconcat [] = mempty
  mconcat (a:as) = a <> mconcat as
