module Data.Fractional(module Data.Fractional) where
import Primitives
import Data.Num

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
--  fromRational :: Rational -> a
  fromDouble :: Double -> a

  recip x = fromDouble 1.0 / x
