-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Fractional(module Data.Fractional) where
import Primitives
import Data.Integral
import Data.Num
import Data.Ord
import Data.Ratio_Type

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  recip x = 1 / x

infixr 8 ^^
(^^) :: forall a b . (Fractional a, Integral b, Ord b) => a -> b -> a
x ^^ n = if n >= 0 then x^n else recip (x^(- n))
