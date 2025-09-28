module Data.Int.Int64(Int64) where
import qualified Prelude()
import Primitives
import Control.Error
import Data.Bits.Base
import Data.Bool
import Data.Bounded
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Function
import Data.Int.Int
import Data.Integer_Type
import Data.Integral
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Data.Real
import Numeric.Show
import Text.Show

instance Num Int64 where
  (+)  = primInt64Add
  (-)  = primInt64Sub
  (*)  = primInt64Mul
  negate x = primInt64Neg x
  abs x = if x < 0 then - x else x
  signum x =
    case compare x 0 of
      LT -> -1
      EQ ->  0
      GT ->  1
  fromInteger = _integerToInt64

instance Integral Int64 where
  quot = primInt64Quot
  rem  = primInt64Rem
  toInteger = _int64ToInteger

instance Bounded Int64 where
  minBound = primWord64ToInt64 ((primWord64Inv (0::Word64) `primWord64Quot` 2) `primWord64Add` 1)
  maxBound = primWord64ToInt64  (primWord64Inv (0::Word64) `primWord64Quot` 2)

instance Real Int64 where
  toRational i = _integerToRational (_int64ToInteger i)

instance Eq Int64 where
  (==) = primInt64EQ
  (/=) = primInt64NE

instance Ord Int64 where
  compare = primInt64Compare
  (<)  = primInt64LT
  (<=) = primInt64LE
  (>)  = primInt64GT
  (>=) = primInt64GE

instance Show Int64 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Int32 where
  readsPrec = readIntegral
-}

instance Enum Int64 where
  succ x = if x == maxBound then error "Int64.succ: overflow" else x + 1
  pred x = if x == minBound then error "Int64.pred: underflow" else x - 1
  toEnum = primIntToInt64
  fromEnum = primInt64ToInt
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Bits Int64 where
  (.&.) = primInt64And
  (.|.) = primInt64Or
  xor   = primInt64Xor
  complement = primInt64Inv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 64 = 0
    | otherwise = x `unsafeShiftL` i
  unsafeShiftL = primInt64Shl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 64 = 0
    | otherwise = x `unsafeShiftR` i
  unsafeShiftR = primInt64Shr
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  bit = bitDefault
  testBit = testBitDefault
  popCount = primInt64Popcount
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Int64 where
  finiteBitSize _ = 64
  countLeadingZeros = primInt64Clz
  countTrailingZeros = primInt64Ctz
