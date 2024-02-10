module Data.Bounded(module Data.Bounded) where
import Prelude()              -- do not import Prelude
import Primitives

class Bounded a where
  minBound :: a
  maxBound :: a
