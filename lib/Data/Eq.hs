module Data.Eq(
  module Data.Eq
  ) where
import Data.Bool

infix 4 ==,/=

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

-- Put Eq instance here, it would be a circular dependency
-- if it were in Data.Bool.
instance Eq Bool where
  False == x  =  not x
  True  == x  =  x
