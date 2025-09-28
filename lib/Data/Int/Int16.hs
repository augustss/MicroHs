module Data.Int.Int16(Int16) where
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

newtype Int16 = I16 Int
  deriving (Typeable, Eq, Ord)

unI16 :: Int16 -> Int
unI16 (I16 x) = x

-- Do sign extension by shifting.
i16 :: Int -> Int16
i16 w = I16 ((w `primIntShl` n) `primIntShr` n)
  where n = _wordSize `primIntSub` 16

bin16 :: (Int -> Int -> Int) -> (Int16 -> Int16 -> Int16)
bin16 op (I16 x) (I16 y) = i16 (x `op` y)

bini16 :: (Int -> Int -> Int) -> (Int16 -> Int -> Int16)
bini16 op (I16 x) y = i16 (x `op` y)

una16 :: (Int -> Int) -> (Int16 -> Int16)
una16 op (I16 x) = i16 (op x)

instance Num Int16 where
  (+)  = bin16 primIntAdd
  (-)  = bin16 primIntSub
  (*)  = bin16 primIntMul
  abs x = x
  signum x = if x < 0 then -1 else if x > 0 then 1 else 0
  fromInteger i = i16 (_integerToInt i)

instance Integral Int16 where
  quot = bin16 primIntQuot
  rem  = bin16 primIntRem
  toInteger = _intToInteger . unI16

instance Bounded Int16 where
  minBound = i16 0x8000
  maxBound = i16 0x7fff

instance Real Int16 where
  toRational = _integerToRational . _intToInteger . unI16

instance Show Int16 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Int16 where
  readsPrec = readIntegral
-}

instance Enum Int16 where
  succ x = if x == maxBound then error "Int16.succ: overflow" else x + 1
  pred x = if x == minBound then error "Int16.pred: underflow" else x - 1
  toEnum = i16
  fromEnum = unI16
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Int)
  enumFromThenTo = coerce (enumFromThenTo @Int)

instance Bits Int16 where
  (.&.) = bin16 primIntAnd
  (.|.) = bin16 primIntOr
  xor   = bin16 primIntXor
  complement = una16 primIntInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 16 = 0
    | True = x `unsafeShiftL` i
  unsafeShiftL = bini16 primIntShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 16 = 0
    | True = x `unsafeShiftR` i
  unsafeShiftR = bini16 primIntShr
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  bit = bitDefault
  testBit = testBitDefault
  popCount (I16 x) = primIntPopcount (x .&. 0xffff)
  zeroBits = 0
  isSigned _ = True

instance FiniteBits Int16 where
  finiteBitSize _ = 16
  countLeadingZeros (I16 x) = primIntClz (x .&. 0xffff) - (_wordSize - 16)
  countTrailingZeros (I16 x) = if x == 0 then 16 else primIntCtz x
