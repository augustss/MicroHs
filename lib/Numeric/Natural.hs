module Numeric.Natural
  ( Natural
  , minusNaturalMaybe
  ) where
import Prelude(); import MiniPrelude
import Data.Integer
import Data.Real
import Control.Exception

newtype Natural = N Integer
  deriving (Eq, Ord)

instance Show Natural where
  showsPrec p (N i) = showsPrec p i

instance Num Natural where
  N x + N y = N (x + y)
  N x - N y | y > x = throw Underflow
  N x * N y = N (x * y)
  abs x = x
  signum x = if x > 0 then 1 else 0
  fromInteger x | x < 0 = throw Underflow
                | otherwise = N x

instance Enum Natural where
  toEnum   = fromInteger . toInteger
  fromEnum = fromInteger . toInteger

instance Integral Natural where
  toInteger (N i) = i
  quotRem (N x) (N y) = (N q, N r) where (q, r) = quotRem x y

instance Real Natural where
  toRational (N i) = toRational i

minusNaturalMaybe :: Natural -> Natural -> Maybe Natural
minusNaturalMaybe x y | x < y = Nothing
                      | otherwise = Just (x - y)
