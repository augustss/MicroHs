module Data.Ratio(
  Ratio, Rational,
  (%),
  numerator, denominator,
  rationalInfinity,
  rationalNaN,
  Rational,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Eq
import Data.Fractional
import Data.Function
--import Data.Int
import Data.Integer
import Data.Integral
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Text.Show

{- in Data.Ratio_Type
data Ratio a = (:%) a a   -- XXX should be strict

type Rational = Ratio Integer
-}

instance forall a . Eq a => Eq (Ratio a) where
  (x :% y) == (x' :% y')  =  x == x' && y == y'

instance forall a . (Integral a, Ord a) => Ord (Ratio a) where
  (x :% y) <= (x' :% y')  =  x * y' <= x' * y
  (x :% y) <  (x' :% y')  =  x * y' <  x' * y
  (x :% y) >= (x' :% y')  =  x * y' >= x' * y
  (x :% y) >  (x' :% y')  =  x * y' >  x' * y

instance forall a . (Integral a) => Num (Ratio a) where
  (x:%y) + (x':%y')   =  reduce (x*y' + x'*y) (y*y')
  (x:%y) - (x':%y')   =  reduce (x*y' - x'*y) (y*y')
  (x:%y) * (x':%y')   =  reduce (x * x') (y * y')
  negate (x:%y)       =  negate x :% y
  abs (x:%y)          =  abs x :% y
  signum (x:%_)       =  signum x :% 1
  fromInteger x       =  fromInteger x :% 1

instance forall a . (Integral a, Ord a) => Fractional (Ratio a) where
  (x:%y) / (x':%y')   = (x*y') % (y*x')
  recip (x:%y)
    | y == 0        = error "Data.Ratio.recip: division by 0"
    | x < 0         = negate y :% negate x
    | otherwise     = y :% x
  fromRational (x:%y) =  fromInteger x % fromInteger y

instance forall a . (Show a) => Show (Ratio a)  where
  showsPrec p (x:%y) = showParen (p > 7) $
                       showsPrec 8 x .
                       showString " % " .
                       showsPrec 8 y

rationalInfinity :: Rational
rationalInfinity = 1 :% 0

rationalNaN :: Rational
rationalNaN = 0 :% 0

infixl 7 %
(%) :: forall a . (Integral a) => a -> a -> Ratio a
x % y = reduce (x * signum y) (abs y)

reduce :: forall a . (Integral a) => a -> a -> Ratio a
reduce x y =
  if y == 0 then
    error "Data.Ratio.%: 0 denominator"
  else
    let d = gcd x y
    in  (x `quot` d) :% (y `quot` d)

numerator :: forall a . Ratio a -> a
numerator (x :% _) = x

denominator :: forall a . Ratio a -> a
denominator (_ :% y) = y
