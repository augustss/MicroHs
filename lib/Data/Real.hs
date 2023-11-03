module Data.Real(module Data.Real) where
import Primitives
import Data.Ratio_Type

class Real a where
  toRational :: a -> Rational
