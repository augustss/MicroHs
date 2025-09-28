module Data.Word.Word64(
  Word64,
  byteSwap64, bitReverse64,
  _word64ToInteger,
  ) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits.Base
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

byteSwap64 :: Word64 -> Word64
byteSwap64 w = (w <<< 56                          ) .|. (w <<< 40 .&. 0x00ff_0000_0000_0000) .|.
               (w <<< 24 .&. 0x0000_ff00_0000_0000) .|. (w <<<  8 .&. 0x0000_00ff_0000_0000) .|.
               (w >>>  8 .&. 0x0000_0000_ff00_0000) .|. (w >>> 24 .&. 0x0000_0000_00ff_0000) .|.
               (w >>> 40 .&. 0x0000_0000_0000_ff00) .|. (w >>> 56                          )
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR

bitReverse64:: Word64 -> Word64
bitReverse64 x0 = x6
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR
        x1 = ((x0 .&. 0x5555555555555555) <<<  1) .|. ((x0 .&. 0xAAAAAAAAAAAAAAAA) >>>  1)
        x2 = ((x1 .&. 0x3333333333333333) <<<  2) .|. ((x1 .&. 0xCCCCCCCCCCCCCCCC) >>>  2)
        x3 = ((x2 .&. 0x0F0F0F0F0F0F0F0F) <<<  4) .|. ((x2 .&. 0xF0F0F0F0F0F0F0F0) >>>  4)
        x4 = ((x3 .&. 0x00FF00FF00FF00FF) <<<  8) .|. ((x3 .&. 0xFF00FF00FF00FF00) >>>  8)
        x5 = ((x4 .&. 0x0000FFFF0000FFFF) <<< 16) .|. ((x4 .&. 0xFFFF0000FFFF0000) >>> 16)
        x6 = ((x5 .&. 0x00000000FFFFFFFF) <<< 32) .|. ((x5 .&. 0xFFFFFFFF00000000) >>> 32)
