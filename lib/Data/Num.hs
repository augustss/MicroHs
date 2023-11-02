module Data.Num(module Data.Num) where
import Primitives

infixl 6 +,-
infixl 7 *

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
--  fromInteger :: Integer -> a
  fromInt :: Int -> a

  negate x = fromInt 0 - x

subtract :: forall a . Num a => a -> a -> a
subtract x y = y - x


