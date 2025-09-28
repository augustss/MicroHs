-- Copyright 2025 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Float(Float) where
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
import Data.Integer.Internal(_integerToFloat, _wordToInteger)
import Data.Integral
import Data.List
import Data.Ord
import Data.Ratio
import Data.Real
import Data.RealFloat
import Data.RealFrac
import Data.Num
import Data.Word.Word
import {-# SOURCE #-} Numeric.FormatFloat(showFloat)
import Numeric.Show(showSignedNeg)
import Text.Show

instance Num Float where
  (+)  = primFloatAdd
  (-)  = primFloatSub
  (*)  = primFloatMul
  negate = primFloatNeg
  abs x = if x < 0.0 then - x else x
  signum x =
    case compare x 0.0 of
      LT -> -1.0
      EQ ->  0.0
      GT ->  1.0
  fromInteger = _integerToFloat

instance Fractional Float where
  (/) = primFloatDiv
  fromRational x =
    let n = numerator x
        d = denominator x
        maxVal = 2 ^ (127::Int)  -- actually bigger, but this converts correctly
    in  if abs n <= maxVal && d < maxVal then
          fromInteger n / fromInteger d
        else
          let (ne, nm) = scaleToMax maxVal n
              (de, dm) = scaleToMax maxVal d
          in  scaleFloat32 (ne - de) (fromInteger nm / fromInteger dm)

-- XXX Very inefficient scaling
-- If we get here the number has > 127 bits, so shifting by 64 is fine.
scaleToMax :: Integer -> Integer -> (Int, Integer)
scaleToMax m = f 0
  where f e x = if abs x < m then (e, x) else f (e+64) (x `shiftR` 64)

instance Eq Float where
  (==) = primFloatEQ
  (/=) = primFloatNE

instance Ord Float where
  (<)  = primFloatLT
  (<=) = primFloatLE
  (>)  = primFloatGT
  (>=) = primFloatGE

instance Show Float where
  showsPrec = showSignedNeg (\ x -> x < 0 || isNegZeroFloat32 x) showFloat

{- in Text.Read.Internal
instance Read Float where
  readsPrec _ = readSigned $ \ r -> [ (primFloatRead s, t) | (s@(c:_), t) <- lex r, isDigit c ]
-}

instance Enum Float where
  succ x = x + 1
  pred x = x - 1
  toEnum n = fromIntegral n
  fromEnum = fromInteger . truncate
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Real Float where
  toRational x | isNaN x = rationalNaN
               | isInfinite x = if x < 0 then -rationalInfinity else rationalInfinity
               | isNegativeZero x = rationalNegativeZero
               | otherwise =
    case decodeFloat x of
      (m, e) -> toRational m * 2^^e

instance RealFrac Float where
  properFraction x =
    case decodeFloat x of
      (m, e) ->   -- x = m * 2^e
        let m' | e < 0     = m `quot` 2^(-e)
               | otherwise = m * 2^e
        in  (fromInteger m', x - fromInteger m')

instance Floating Float where
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

foreign import ccall "logf"  clog  :: Float -> Float
foreign import ccall "expf"  cexp  :: Float -> Float
foreign import ccall "sqrtf" csqrt :: Float -> Float
foreign import ccall "sinf"  csin  :: Float -> Float
foreign import ccall "cosf"  ccos  :: Float -> Float
foreign import ccall "tanf"  ctan  :: Float -> Float
foreign import ccall "asinf" casin :: Float -> Float
foreign import ccall "acosf" cacos :: Float -> Float
foreign import ccall "atanf" catan :: Float -> Float
foreign import ccall "atan2f" catan2 :: Float -> Float -> Float
foreign import ccall "scalbnf" cscalbn :: Float -> Int -> Float

-- Assumes 32/64 bit floats
instance RealFloat Float where
  floatRadix     _ = 2
  floatDigits    _ = 24
  floatRange     _ = (-125,128)
  decodeFloat      = decodeFloat32
  encodeFloat      = encodeFloat32
  isNaN            = isNaNFloat32
  isInfinite       = isInfFloat32
  isDenormalized   = isDenFloat32
  isNegativeZero   = isNegZeroFloat32
  isIEEE         _ = True
  atan2 x y        = catan2 x y
  scaleFloat       = scaleFloat32

decodeFloat32 :: Float -> (Integer, Int)
decodeFloat32 x =
  let xw   = primWordFromFloatRaw x
      sign =  xw .&. 0x80000000
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
      neg  = if sign /= 0 then negate else id
      -- Haskell promises that the mantissa is normalized
      adjDenorm m e | abs m < (1 `shiftL` 23) = adjDenorm (2 * m) (e - 1)
                    | otherwise = (m, e)
  in  if expn == 0 then
        -- denormalized or 0
        if mant == 0 then
          (0, 0)  -- 0
        else
          adjDenorm (neg (_wordToInteger mant)) (-149)  -- denormalized
      else if expn == 0xff then
        -- infinity or NaN
        error "decodeFloat32: inf or NaN"
      else
        -- ordinary number, add hidden bit
        -- mant is offset-127, and assumes scaled mantissa (thus -23)
        (neg (_wordToInteger (mant .|. 0x00800000)),
         primWordToInt expn - 127 - 23)

isNaNFloat32 :: Float -> Bool
isNaNFloat32 x =
  let xw   = primWordFromFloatRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0xff && mant /= 0

isInfFloat32 :: Float -> Bool
isInfFloat32 x =
  let xw   = primWordFromFloatRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0xff && mant == 0

isDenFloat32 :: Float -> Bool
isDenFloat32 x =
  let xw   = primWordFromFloatRaw x
      expn = (xw .&. 0x7fffffff) `shiftR` 23
      mant =  xw .&. 0x007fffff
  in  expn == 0 && mant /= 0

isNegZeroFloat32 :: Float -> Bool
isNegZeroFloat32 x =
  primWordFromFloatRaw x == 0x80000000

scaleFloat32 :: Int -> Float -> Float
scaleFloat32 n x = cscalbn x n

encodeFloat32 :: Integer -> Int -> Float
encodeFloat32 mant expn = scaleFloat32 expn (fromInteger mant)
