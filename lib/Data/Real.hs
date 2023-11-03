module Data.Real(module Data.Real) where
import Primitives
import Data.Num
import Data.Int
import Data.Ratio

class  (Num a, Ord a) => Real a  where
  toRational :: a -> Rational
