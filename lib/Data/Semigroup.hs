module Data.Semigroup(module Data.Semigroup) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Eq
import Data.Integral
import Data.List_Type
import Data.List.NonEmpty_Type
import Data.Num
import Data.Ord

infixr 6 <>
class Semigroup a where
  (<>)    :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes  :: (Integral b, Ord b) => b -> a -> a

  sconcat (a :| as) = go a as
    where go b (c:cs) = b <> go c cs
          go b []     = b

  stimes y0 x0
    | y0 <= 0   = error "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | y `rem` 2 == 0 = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (y `quot` 2) x
      g x y z
        | y `rem` 2 == 0 = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (y `quot` 2) (x <> z)

stimesIdempotent :: (Integral b, Ord b) => b -> a -> a
stimesIdempotent n x =
  if n <= 0 then error "stimesIdempotent: positive multiplier expected"
  else x
