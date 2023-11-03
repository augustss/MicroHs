-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Fractional(module Data.Fractional) where
import Primitives
import Data.Ratio_Type
import Data.Num

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a

  recip x = 1 / x
