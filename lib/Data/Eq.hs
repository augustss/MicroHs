module Data.Eq(
  module Data.Eq
  ) where
import Data.Bool

infix 4 ==,/=

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

{-
instance Eq Int where
  (==) = primIntEq

instance Eq Char where
  (==) = primCharEq
-}
