module Data.Float(Float) where
import Prelude()
import Primitives
import Data.Bool
import Data.Eq
import Data.FloatW
import Data.Floating
import Data.Fractional
import Data.Function
import Data.List
import Data.Num
import Data.Ord
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Text.Show

-- XXX I should really implement newtype deriving...

newtype Float = D FloatW
unD :: Float -> FloatW
unD (D x) = x

un :: (FloatW -> FloatW) -> (Float -> Float)
un f x = D (f (unD x))

bin :: (FloatW -> FloatW -> FloatW) -> (Float -> Float -> Float)
bin f x y = D (f (unD x) (unD y))

cmp :: (FloatW -> FloatW -> Bool) -> (Float -> Float -> Bool)
cmp f x y = f (unD x) (unD y)

instance Num Float where
  (+) = bin (+)
  (-) = bin (-)
  (*) = bin (*)
  negate = un negate
  abs = un abs
  signum = un signum
  fromInteger = D . fromInteger

instance Fractional Float where
  (/) = bin (/)
  fromRational = D . fromRational

instance Eq Float where
  (==) = cmp (==)
  (/=) = cmp (/=)

instance Ord Float where
  (<) = cmp (<)
  (<=) = cmp (<=)
  (>) = cmp (>)
  (>=) = cmp (>=)

instance Show Float where
  showsPrec p = showsPrec p . unD

{- in Text.Read.Internal
instance Read Float where
  readsPrec p = map (\ (x, s) -> (D x, s)) . readsPrec p
-}

instance Real Float where
  toRational = toRational . unD

instance RealFrac Float where
  properFraction x = (a, D b) where (a, b) = properFraction (unD x)

instance Floating Float where
  pi = D pi
  log = un log
  exp = un exp
  sqrt = un sqrt
  sin = un sin
  cos = un cos
  tan = un tan
  asin = un asin
  acos = un acos
  atan = un atan

instance RealFloat Float where
  floatRadix = floatRadix . unD
  floatDigits = floatDigits . unD
  floatRange = floatRange . unD
  decodeFloat = decodeFloat . unD
  encodeFloat e = D . encodeFloat e
  isNaN = isNaN . unD
  isInfinite = isInfinite . unD
  isDenormalized = isDenormalized . unD
  isNegativeZero = isNegativeZero . unD
  isIEEE = isIEEE . unD
  atan2 = bin atan2
