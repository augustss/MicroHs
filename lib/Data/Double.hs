-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Double(Double) where
import Primitives
import Control.Error
import Data.Bits
import Data.Bool
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Function
import Data.Integer
import Data.Ord
import Data.Ratio
import Data.Real
import Data.RealFloat
import Data.Num
import Data.Word
import Text.Show

instance Num Double where
  (+)  = primDoubleAdd
  (-)  = primDoubleSub
  (*)  = primDoubleMul
  negate = primDoubleNeg
  abs x = if x < 0.0 then negate x else x
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
  fromRational x = fromInteger (numerator x) `primDoubleDiv` fromInteger (denominator x)

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

instance Real Double where
  toRational x =
    let (m, e) = decodeDouble x
    in  toRational m * 2^^e

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
  floatDigits    _ = 53
  floatRange     _ = (-1021,1024)
  decodeFloat      = decodeDouble
  encodeFloat      = encodeDouble
  isNaN            = isNaNDouble
  isInfinite       = isInfDouble
  isDenormalized   = isDenDouble
  isNegativeZero   = isNegZeroDouble
  isIEEE         _ = True
  atan2 x y        = primPerformIO (catan2 x y)

decodeDouble :: Double -> (Integer, Int)
decodeDouble x =
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

isNaNDouble :: Double -> Bool
isNaNDouble x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant /= 0

isInfDouble :: Double -> Bool
isInfDouble x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0x7ff && mant == 0

isDenDouble :: Double -> Bool
isDenDouble x =
  let xw   = primWordFromDoubleRaw x
      expn = (xw .&. 0x7fffffffffffffff) `shiftR` 52
      mant =  xw .&. 0x000fffffffffffff
  in  expn == 0 && mant /= 0

isNegZeroDouble :: Double -> Bool
isNegZeroDouble x =
  let xw   = primWordFromDoubleRaw x
      sign = xw .&. 0x8000000000000000
      rest = xw .&. 0x7fffffffffffffff
  in  sign /= 0 && rest == 0

-- Simple (and sometimes wrong) encoder
encodeDouble :: Integer -> Int -> Double
encodeDouble mant expn = fromInteger mant * 2^^expn
