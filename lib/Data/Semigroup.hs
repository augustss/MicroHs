module Data.Semigroup(Data.Semigroup) where
import Primitive

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a
