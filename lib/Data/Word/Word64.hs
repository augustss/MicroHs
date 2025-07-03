module Data.Word.Word64(
  Word64,
  _word64ToInteger,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits
import Data.Bool
import Data.Bounded
import Data.Enum
import Data.Eq
import Data.Function
import Data.Integer_Type
import Data.Integral
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Data.Real
import Numeric.Show
import Text.Show

instance Num Word64 where
  (+)  = primWord64Add
  (-)  = primWord64Sub
  (*)  = primWord64Mul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = _integerToWord64 i

instance Integral Word64 where
  quot = primWord64Quot
  rem  = primWord64Rem
  toInteger = _word64ToInteger

instance Bounded Word64 where
  minBound = 0
  maxBound = 0xffff_ffff_ffff_ffff

instance Real Word64 where
  toRational = _integerToRational . _word64ToInteger

instance Show Word64 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word64 where
  readsPrec = readIntegral
-}

instance Enum Word64 where
  succ x = if x == maxBound then error "Word64.succ: overflow" else x + 1
  pred x = if x == minBound then error "Word64.pred: underflow" else x - 1
  toEnum = primWordToWord64 . primIntToWord
  fromEnum = primWordToInt . primWord64ToWord
  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
  enumFromTo = numericEnumFromTo
  enumFromThenTo = numericEnumFromThenTo

instance Eq Word64 where
  (==) = primWord64EQ
  (/=) = primWord64NE

instance Ord Word64 where
  compare = primWord64Compare
  (<)  = primWord64LT
  (<=) = primWord64LE
  (>)  = primWord64GT
  (>=) = primWord64GE

instance Bits Word64 where
  (.&.) = primWord64And
  (.|.) = primWord64Or
  xor   = primWord64Xor
  complement = primWord64Inv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 64 = 0
    | otherwise = x `unsafeShiftL` i
  unsafeShiftL = primWord64Shl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 64 = 0
    | otherwise = x `unsafeShiftR` i
  unsafeShiftR = primWord64Shr
  bitSizeMaybe _ = Just 64
  bitSize _ = 64
  bit = bitDefault
  testBit = testBitDefault
  popCount = primWord64Popcount
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Word64 where
  finiteBitSize _ = 64
  countLeadingZeros = primWord64Clz
  countTrailingZeros = primWord64Ctz
