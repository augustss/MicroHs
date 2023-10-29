module Data.Monoid(module Data.Monoid) where
import Primitives
import Data.Semigroup

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)
