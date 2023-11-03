-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integral(module Data.Integral) where
import Primitives
import Data.Eq
import Data.Integer_Type
import Data.Num

infixl 7 `quot`,`rem`

class {-(Real a, Enum a) => -} (Eq a, Num a) => Integral a where
  quot      :: a -> a -> a
  rem       :: a -> a -> a
  div       :: a -> a -> a
  mod       :: a -> a -> a
  quotRem   :: a -> a -> (a, a)
  divMod    :: a -> a -> (a, a)
  toInteger :: a -> Integer

  n `quot` d       =  q  where (q,r) = quotRem n d
  n `rem` d        =  r  where (q,r) = quotRem n d
  n `div` d        =  q  where (q,r) = divMod n d
  n `mod` d        =  r  where (q,r) = divMod n d
  divMod n d       =  if signum r == negate (signum d) then (q - 1, r + d) else qr
                        where qr@(q,r) = quotRem n d
  quotRem n d      = (quot n d, rem n d)
