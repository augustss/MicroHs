module Data.Int.Int32(Int32) where
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
import {-# SOURCE #-} Data.Typeable
import Numeric.Show
import Text.Show

newtype Int32 = I32 Int
  deriving (Typeable, Eq, Ord)

unI32 :: Int32 -> Int
unI32 (I32 x) = x

-- Do sign extension by shifting.
i32 :: Int -> Int32
i32 w = if n == 0 then I32 w else I32 ((w `primIntShl` n) `primIntShr` n)
  where n = _wordSize `primIntSub` 32

bin32 :: (Int -> Int -> Int) -> (Int32 -> Int32 -> Int32)
bin32 op (I32 x) (I32 y) = i32 (x `op` y)

bini32 :: (Int -> Int -> Int) -> (Int32 -> Int -> Int32)
bini32 op (I32 x) y = i32 (x `op` y)

una32 :: (Int -> Int) -> (Int32 -> Int32)
una32 op (I32 x) = i32 (op x)

instance Num Int32 where
  (+)  = bin32 primIntAdd
  (-)  = bin32 primIntSub
  (*)  = bin32 primIntMul
  abs x = x
  signum x = if x < 0 then -1 else if x > 0 then 1 else 0
  fromInteger i = i32 (_integerToInt i)

instance Integral Int32 where
  quot = bin32 primIntQuot
  rem  = bin32 primIntRem
  toInteger = _intToInteger . unI32

instance Bounded Int32 where
  minBound = i32 0x80000000
  maxBound = i32 0x7fffffff

instance Real Int32 where
  toRational = _integerToRational . _intToInteger . unI32

instance Show Int32 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Int32 where
  readsPrec = readIntegral
-}

instance Enum Int32 where
  succ x = if x == maxBound then error "Int32.succ: overflow" else x + 1
  pred x = if x == minBound then error "Int32.pred: underflow" else x - 1
  toEnum = i32
  fromEnum = unI32
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Int)
  enumFromThenTo = coerce (enumFromThenTo @Int)

instance Bits Int32 where
  (.&.) = bin32 primIntAnd
  (.|.) = bin32 primIntOr
  xor   = bin32 primIntXor
  complement = una32 primIntInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 32 = 0
    | True = x `unsafeShiftL` i
  unsafeShiftL = bini32 primIntShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 32 = 0
    | True = x `unsafeShiftR` i
  unsafeShiftR = bini32 primIntShr
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  bit = bitDefault
  testBit = testBitDefault
  popCount (I32 x) = primIntPopcount (x .&. 0xffffffff)
  zeroBits = 0
  isSigned _ = True

instance FiniteBits Int32 where
  finiteBitSize _ = 32
  countLeadingZeros (I32 x) = primIntClz (x .&. 0xffffffff) - (_wordSize - 32)
  countTrailingZeros (I32 x) = if x == 0 then 32 else primIntCtz x
