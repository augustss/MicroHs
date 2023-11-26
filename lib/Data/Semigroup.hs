module Data.Semigroup(module Data.Semigroup) where
import Primitives

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a
