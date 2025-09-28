-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(Double) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits.Base
import Data.Bool
import Data.Char
import Data.Enum
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Function
import Data.Integer
import Data.Integer.Internal(_integerToDouble, _wordToInteger)
import Data.Integral
import Data.List
import Data.Ord
import Data.Ratio
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Num
import Data.Word.Word64
import {-# SOURCE #-} Numeric.FormatFloat(showFloat)
import Numeric.Show(showSignedNeg)
import Text.Show

--
-- This is a floating point type that is a wide as the word width of the machine.
--

instance Num Double where
  (+)  = primDoubleAdd
  (-)  = primDoubleSub
  (*)  = primDoubleMul
  negate = primDoubleNeg
  abs x = if x < 0.0 then - x else x
  signum x =
    case compare x 0.0 of
      LT -> -1.0
      EQ ->  0.0
      GT ->  1.0
  fromInteger = _integerToDouble

instance Fractional Double where
  (/) = primDoubleDiv
  fromRational x =
    let n = numerator x
        d = denominator x
        maxVal = 2 ^ (1023::Int)  -- actually bigger, but this converts correctly
    in  if abs n <= maxVal && d < maxVal then
          fromInteger n / fromInteger d
        else
          let (ne, nm) = scaleToMax maxVal n
              (de, dm) = scaleToMax maxVal d
          in  scaleFloat64 (ne - de) (fromInteger nm / fromInteger dm)

-- XXX Very inefficient scaling
-- If we get here the number has > 1023 bits, so shifting by 256 is fine.
scaleToMax :: Integer -> Integer -> (Int, Integer)
scaleToMax m = f 0
  where f e x = if abs x < m then (e, x) else f (e+256) (x `shiftR` 256)

instance Eq Double where
  (==) = primDoubleEQ
  (/=) = primDoubleNE

instance Ord Double where
  (<)  = primDoubleLT
  (<=) = primDoubleLE
  (>)  = primDoubleGT
  (>=) = primDoubleGE

instance Show Double where
  showsPrec = showSignedNeg (\ x -> x < 0 || isNegZeroFloat64 x) showFloat

{- in Text.Read.Internal
instance Read Double where
  readsPrec _ = readSigned $ \ r -> [ (primDoubleRead s, t) | (s@(c:_), t) <- lex r, isDigit c ]
-}

instance Enum Double where
  succ x = x + 1
  pred x = x - 1
  toEnum n = fromIntegral n
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Real Double where
  toRational x | isNaN x = rationalNaN
               | isInfinite x = if x < 0 then -rationalInfinity else rationalInfinity
               | isNegativeZero x = rationalNegativeZero
               | otherwise =
    case decodeFloat x of
      (m, e) -> toRational m * 2^^e

instance RealFrac Double where
  properFraction x =
    case decodeFloat x of
      (m, e) ->   -- x = m * 2^e
        let m' | e < 0     = m `quot` 2^(-e)
               | otherwise = m * 2^e
        in  (fromInteger m', x - fromInteger m')

instance Floating Double where
  pi     = 3.141592653589793
  log  x = clog x
  exp  x = cexp x
  sqrt x = csqrt x
  sin  x = csin x
  cos  x = ccos x
  tan  x = ctan x
  asin x = casin x
  acos x = cacos x
  atan x = catan x

foreign import ccall "log"  clog  :: Double -> Double
foreign import ccall "exp"  cexp  :: Double -> Double
foreign import ccall "sqrt" csqrt :: Double -> Double
foreign import ccall "sin"  csin  :: Double -> Double
foreign import ccall "cos"  ccos  :: Double -> Double
foreign import ccall "tan"  ctan  :: Double -> Double
foreign import ccall "asin" casin :: Double -> Double
foreign import ccall "acos" cacos :: Double -> Double
foreign import ccall "atan" catan :: Double -> Double
foreign import ccall "atan2" catan2 :: Double -> Double -> Double
foreign import ccall "scalbn" cscalbn :: Double -> Int -> Double

-- Assumes 32/64 bit floats
instance RealFloat Double where
  floatRadix     _ = 2
  floatDigits    _ = 53
  floatRange     _ = (-1021,1024)
  decodeFloat      = decodeFloat64
  encodeFloat      = encodeFloat64
  isNaN            = isNaNFloat64
  isInfinite       = isInfFloat64
  isDenormalized   = isDenFloat64
  isNegativeZero   = isNegZeroFloat64
  isIEEE         _ = True
  atan2 x y        = catan2 x y
  scaleFloat       = scaleFloat64

decodeFloat64 :: Double -> (Integer, Int)
decodeFloat64 x =
  let xw   = primWord64FromDoubleRaw x
      sign =  xw .&. 0x8000000000000000
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
      neg  = if sign /= 0 then negate else id
      -- Haskell promises that the mantissa is normalized
      adjDenorm m e | abs m < (1 `shiftL` 52) = adjDenorm (2 * m) (e - 1)
                    | otherwise = (m, e)
  in  if expn == 0 then
        -- denormalized or 0
        if mant == 0 then
          (0, 0)  -- 0
        else
          adjDenorm (neg (_word64ToInteger mant)) (-1074)  -- denormalized
      else if expn == 0x7ff then
        -- infinity or NaN
        --(0, 0)
        error "decodeFloat64: inf or NaN"
      else
        -- ordinary number, add hidden bit
        -- mant is offset-1023, and assumes scaled mantissa (thus -52)
        (neg (_word64ToInteger (mant .|. 0x0010000000000000)),
         primWordToInt (primWord64ToWord expn) - 1023 - 52)

isNaNFloat64 :: Double -> Bool
isNaNFloat64 x =
  let xw   = primWord64FromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant /= 0

isInfFloat64 :: Double -> Bool
isInfFloat64 x =
  let xw   = primWord64FromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant == 0

isDenFloat64 :: Double -> Bool
isDenFloat64 x =
  let xw   = primWord64FromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0 && mant /= 0

isNegZeroFloat64 :: Double -> Bool
isNegZeroFloat64 x =
  primWord64FromDoubleRaw x == 0x8000000000000000

scaleFloat64 :: Int -> Double -> Double
scaleFloat64 n x = cscalbn x n

encodeFloat64 :: Integer -> Int -> Double
encodeFloat64 mant expn = scaleFloat64 expn (fromInteger mant)
