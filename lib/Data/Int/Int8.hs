module Data.Int.Int8(Int8) where
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

newtype Int8 = I8 Int
  deriving (Typeable, Eq, Ord)

unI8 :: Int8 -> Int
unI8 (I8 x) = x

-- Do sign extension by shifting.
i8 :: Int -> Int8
i8 w = I8 ((w `primIntShl` n) `primIntShr` n)
  where n = _wordSize `primIntSub` 8

bin8 :: (Int -> Int -> Int) -> (Int8 -> Int8 -> Int8)
bin8 op (I8 x) (I8 y) = i8 (x `op` y)

bini8 :: (Int -> Int -> Int) -> (Int8 -> Int -> Int8)
bini8 op (I8 x) y = i8 (x `op` y)

una8 :: (Int -> Int) -> (Int8 -> Int8)
una8 op (I8 x) = i8 (op x)

instance Num Int8 where
  (+)  = bin8 primIntAdd
  (-)  = bin8 primIntSub
  (*)  = bin8 primIntMul
  abs x = x
  signum x = if x < 0 then -1 else if x > 0 then 1 else 0
  fromInteger i = i8 (_integerToInt i)

instance Integral Int8 where
  quot = bin8 primIntQuot
  rem  = bin8 primIntRem
  toInteger = _intToInteger . unI8

instance Bounded Int8 where
  minBound = i8 0x80
  maxBound = i8 0x7f

instance Real Int8 where
  toRational = _integerToRational . _intToInteger . unI8

instance Show Int8 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Int8 where
  readsPrec = readIntegral
-}

instance Enum Int8 where
  succ x = if x == maxBound then error "Int8.succ: overflow" else x + 1
  pred x = if x == minBound then error "Int8.pred: underflow" else x - 1
  toEnum = i8
  fromEnum = unI8
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Int)
  enumFromThenTo = coerce (enumFromThenTo @Int)

instance Bits Int8 where
  (.&.) = bin8 primIntAnd
  (.|.) = bin8 primIntOr
  xor   = bin8 primIntXor
  complement = una8 primIntInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 8 = 0
    | True = x `unsafeShiftL` i
  unsafeShiftL = bini8 primIntShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 8 = 0
    | True = x `unsafeShiftR` i
  unsafeShiftR = bini8 primIntShr
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  bit = bitDefault
  testBit = testBitDefault
  popCount (I8 x) = primIntPopcount (x .&. 0xff)
  zeroBits = 0
  isSigned _ = True

instance FiniteBits Int8 where
  finiteBitSize _ = 8
  countLeadingZeros (I8 x) = primIntClz (x .&. 0xff) - (_wordSize - 8)
  countTrailingZeros (I8 x) = if x == 0 then 8 else primIntCtz x
