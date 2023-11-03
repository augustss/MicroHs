module Data.Bounded(module Data.Bounded) where
import Primitives

class Bounded a where
  minBound :: a
  maxBound :: a
