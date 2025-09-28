-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word.Word16(Word16, byteSwap16, bitReverse16) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bits.Base
import Data.Bool
import Data.Bounded
import Data.Char
import Data.Coerce
import Data.Enum
import Data.Eq
import Data.Function
--import Data.Int()  -- instances only
import Data.Integer_Type
import Data.Integral
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Data.Real
import {-# SOURCE #-} Data.Typeable
import Data.Word.Word
import Numeric.Show
import Text.Show

newtype Word16 = W16 Word
  deriving (Typeable, Eq, Ord)

unW16 :: Word16 -> Word
unW16 (W16 x) = x

w16 :: Word -> Word16
w16 w = W16 (w .&. 0xffff)

bin16 :: (Word -> Word -> Word) -> (Word16 -> Word16 -> Word16)
bin16 op (W16 x) (W16 y) = w16 (x `op` y)

bini16 :: (Word -> Int -> Word) -> (Word16 -> Int -> Word16)
bini16 op (W16 x) y = w16 (x `op` y)

una16 :: (Word -> Word) -> (Word16 -> Word16)
una16 op (W16 x) = w16 (op x)

instance Num Word16 where
  (+)  = bin16 primWordAdd
  (-)  = bin16 primWordSub
  (*)  = bin16 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w16 (_integerToWord i)

instance Integral Word16 where
  quot = bin16 primWordQuot
  rem  = bin16 primWordRem
  toInteger = _wordToInteger . unW16

instance Bounded Word16 where
  minBound = w16 0
  maxBound = w16 0xffff

instance Real Word16 where
  toRational = _integerToRational . _wordToInteger . unW16

instance Show Word16 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word16 where
  readsPrec = readIntegral
-}

instance Enum Word16 where
  succ x = if x == maxBound then error "Word16.succ: overflow" else x + 1
  pred x = if x == minBound then error "Word16.pred: underflow" else x - 1
  toEnum = w16 . primIntToWord
  fromEnum = primWordToInt . unW16
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Word)
  enumFromThenTo = coerce (enumFromThenTo @Word)

instance Bits Word16 where
  (.&.) = bin16 primWordAnd
  (.|.) = bin16 primWordOr
  xor   = bin16 primWordXor
  complement = una16 primWordInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 16 = 0
    | otherwise = x `unsafeShiftL` i
  unsafeShiftL = bini16 primWordShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 16 = 0
    | otherwise = x `unsafeShiftR` i
  unsafeShiftR = bini16 primWordShr
  bitSizeMaybe _ = Just 16
  bitSize _ = 16
  bit = bitDefault
  testBit = testBitDefault
  popCount = primWordPopcount . unW16
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Word16 where
  finiteBitSize _ = 16
  countLeadingZeros (W16 x) = primWordClz x - (_wordSize - 16)
  countTrailingZeros (W16 x) = if x == 0 then 16 else primWordCtz x

byteSwap16 :: Word16 -> Word16
byteSwap16 w = (w <<< 8) .|. (w >>> 8)
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR

bitReverse16 :: Word16 -> Word16
bitReverse16 x0 = x4
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR
        x1 = ((x0 .&. 0x5555) <<<  1) .|. ((x0 .&. 0xAAAA) >>>  1)
        x2 = ((x1 .&. 0x3333) <<<  2) .|. ((x1 .&. 0xCCCC) >>>  2)
        x3 = ((x2 .&. 0x0F0F) <<<  4) .|. ((x2 .&. 0xF0F0) >>>  4)
        x4 = ((x3 .&. 0x00FF) <<<  8) .|. ((x3 .&. 0xFF00) >>>  8)
