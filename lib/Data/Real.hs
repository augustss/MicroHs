module Data.Real(module Data.Real) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Num
import Data.Ord
import Data.Ratio_Type

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
