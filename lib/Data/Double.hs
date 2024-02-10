-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(Double) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits
import Data.Bool
import Data.Char
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Function
import Data.Integer
import Data.List
import Data.Ord
import Data.Ratio
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Num
import Data.Word
import Text.Read
import Text.Read.Numeric
import Text.Show

--
-- NOTE: On 32 bit platforms the MicroHs Double type is actually 32 bit floats.
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
  -- This version of fromRational can go horribly wrong
  -- if the integers are bigger than can be represented in a Double.
  -- It'll do for now.
  fromRational x | x == rationalNaN = 0/0
                 | x == rationalInfinity = 1/0
                 | x == -rationalInfinity = (-1)/0
                 | otherwise =
    fromInteger (numerator x) / fromInteger (denominator x)

instance Eq Double where
  (==) = primDoubleEQ
  (/=) = primDoubleNE

instance Ord Double where
  (<)  = primDoubleLT
  (<=) = primDoubleLE
  (>)  = primDoubleGT
  (>=) = primDoubleGE
  
-- For now, cheat and call C
instance Show Double where
  show = primDoubleShow

instance Read Double where
  readsPrec _ = readSigned $ \ r -> [ (primDoubleRead s, t) | (s@(c:_), t) <- lex r, isDigit c ]

instance Real Double where
  toRational x | isNaN x = rationalNaN
               | isInfinite x = if x < 0 then -rationalInfinity else rationalInfinity
               | otherwise =
    case decodeFloat x of
      (m, e) -> toRational m * 2^^e

instance RealFrac Double where
  properFraction _ = error "Double.properFraction not implemented"

instance Floating Double where
  pi     = 3.141592653589793
  log  x = primPerformIO (clog x)
  exp  x = primPerformIO (cexp x)
  sqrt x = primPerformIO (csqrt x)
  sin  x = primPerformIO (csin x)
  cos  x = primPerformIO (ccos x)
  tan  x = primPerformIO (ctan x)
  asin x = primPerformIO (casin x)
  acos x = primPerformIO (cacos x)
  atan x = primPerformIO (catan x)

foreign import ccall "log"  clog  :: Double -> IO Double
foreign import ccall "exp"  cexp  :: Double -> IO Double
foreign import ccall "sqrt" csqrt :: Double -> IO Double
foreign import ccall "sin"  csin  :: Double -> IO Double
foreign import ccall "cos"  ccos  :: Double -> IO Double
foreign import ccall "tan"  ctan  :: Double -> IO Double
foreign import ccall "asin" casin :: Double -> IO Double
foreign import ccall "acos" cacos :: Double -> IO Double
foreign import ccall "atan" catan :: Double -> IO Double
foreign import ccall "atan2" catan2 :: Double -> Double -> IO Double

-- Assumes 64 bit floats
instance RealFloat Double where
  floatRadix     _ = 2
  floatDigits    _ = flt 24 53
  floatRange     _ = flt (-125,128) (-1021,1024)
  decodeFloat      = flt decodeFloat32 decodeFloat64
  encodeFloat      = flt encodeFloat32 encodeFloat64
  isNaN            = flt isNaNFloat32 isNaNFloat64
  isInfinite       = flt isInfFloat32 isInfFloat64
  isDenormalized   = flt isDenFloat32 isDenFloat64
  isNegativeZero   = flt isNegZeroFloat32 isNegZeroFloat64
  isIEEE         _ = True
  atan2 x y        = primPerformIO (catan2 x y)

flt :: forall a . a -> a -> a
flt f d | _wordSize == 32 = f
        | otherwise       = d

decodeFloat64 :: Double -> (Integer, Int)
decodeFloat64 x =
  let xw   = primWordFromDoubleRaw x
      sign =  xw .&. 0x8000000000000000
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
      neg  = if sign /= 0 then negate else id
  in  if expn == 0 then
        -- subnormal or 0
        (neg (_wordToInteger mant), 0)
      else if expn == 0x7ff then
        -- infinity or NaN
        (0, 0)
      else
        -- ordinary number, add hidden bit
        -- mant is offset-1023, and assumes scaled mantissa (thus -52)
        (neg (_wordToInteger (mant .|. 0x0010000000000000)),
         primWordToInt expn - 1023 - 52)

isNaNFloat64 :: Double -> Bool
isNaNFloat64 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant /= 0

isInfFloat64 :: Double -> Bool
isInfFloat64 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant == 0

isDenFloat64 :: Double -> Bool
isDenFloat64 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0 && mant /= 0

isNegZeroFloat64 :: Double -> Bool
isNegZeroFloat64 x =
  let xw   = primWordFromDoubleRaw x
      sign = xw .&. 0x8000000000000000
      rest = xw .&. 0x7fffffffffffffff
  in  sign /= 0 && rest == 0

-- Simple (and sometimes wrong) encoder
encodeFloat64 :: Integer -> Int -> Double
encodeFloat64 mant expn = fromInteger mant * 2^^expn

decodeFloat32 :: Double -> (Integer, Int)
decodeFloat32 x =
  let xw   = primWordFromDoubleRaw x
      sign =  xw .&. 0x80000000
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
      neg  = if sign /= 0 then negate else id
  in  if expn == 0 then
        -- subnormal or 0
        (neg (_wordToInteger mant), 0)
      else if expn == 0xff then
        -- infinity or NaN
        (0, 0)
      else
        -- ordinary number, add hidden bit
        -- mant is offset-1023, and assumes scaled mantissa (thus -52)
        (neg (_wordToInteger (mant .|. 0x00400000)),
         primWordToInt expn - 127 - 22)

isNaNFloat32 :: Double -> Bool
isNaNFloat32 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0xff && mant /= 0

isInfFloat32 :: Double -> Bool
isInfFloat32 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0x7ff && mant == 0

isDenFloat32 :: Double -> Bool
isDenFloat32 x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0 && mant /= 0

isNegZeroFloat32 :: Double -> Bool
isNegZeroFloat32 x =
  let xw   = primWordFromDoubleRaw x
      sign = xw .&. 0x80000000
      rest = xw .&. 0x7fffffff
  in  sign /= 0 && rest == 0

-- Simple (and sometimes wrong) encoder
encodeFloat32 :: Integer -> Int -> Double
encodeFloat32 mant expn = fromInteger mant * 2^^expn
