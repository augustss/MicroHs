-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word.Word32(Word32, byteSwap32, bitReverse32) where
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

newtype Word32 = W32 Word
  deriving (Typeable, Eq, Ord)

unW32 :: Word32 -> Word
unW32 (W32 x) = x

w32 :: Word -> Word32
w32 w = if _wordSize == 32 then W32 w else W32 (w .&. 0xffffffff)

bin32 :: (Word -> Word -> Word) -> (Word32 -> Word32 -> Word32)
bin32 op (W32 x) (W32 y) = w32 (x `op` y)

bini32 :: (Word -> Int -> Word) -> (Word32 -> Int -> Word32)
bini32 op (W32 x) y = w32 (x `op` y)

una32 :: (Word -> Word) -> (Word32 -> Word32)
una32 op (W32 x) = w32 (op x)

instance Num Word32 where
  (+)  = bin32 primWordAdd
  (-)  = bin32 primWordSub
  (*)  = bin32 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w32 (_integerToWord i)

instance Integral Word32 where
  quot = bin32 primWordQuot
  rem  = bin32 primWordRem
  toInteger = _wordToInteger . unW32

instance Bounded Word32 where
  minBound = w32 0
  maxBound = w32 0xffffffff

instance Real Word32 where
  toRational = _integerToRational . _wordToInteger . unW32

instance Show Word32 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word32 where
  readsPrec = readIntegral
-}

instance Enum Word32 where
  succ x = if x == maxBound then error "Word32.succ: overflow" else x + 1
  pred x = if x == minBound then error "Word32.pred: underflow" else x - 1
  toEnum = w32 . primIntToWord
  fromEnum = primWordToInt . unW32
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Word)
  enumFromThenTo = coerce (enumFromThenTo @Word)

instance Bits Word32 where
  (.&.) = bin32 primWordAnd
  (.|.) = bin32 primWordOr
  xor   = bin32 primWordXor
  complement = una32 primWordInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 32 = 0
    | otherwise = x `unsafeShiftL` i
  unsafeShiftL = bini32 primWordShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 32 = 0
    | otherwise = x `unsafeShiftR` i
  unsafeShiftR = bini32 primWordShr
  bitSizeMaybe _ = Just 32
  bitSize _ = 32
  bit = bitDefault
  testBit = testBitDefault
  popCount = primWordPopcount . unW32
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Word32 where
  finiteBitSize _ = 32
  countLeadingZeros (W32 x) = primWordClz x - (_wordSize - 32)
  countTrailingZeros (W32 x) = if x == 0 then 32 else primWordCtz x

byteSwap32 :: Word32 -> Word32
byteSwap32 w = (w <<< 24) .|. (w <<< 8 .&. 0x00ff0000) .|. (w >>> 8 .&. 0x0000ff00) .|. (w >>> 24)
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR

bitReverse32 :: Word32 -> Word32
bitReverse32 x0 = x5
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR
        x1 = ((x0 .&. 0x55555555) <<<  1) .|. ((x0 .&. 0xAAAAAAAA) >>>  1)
        x2 = ((x1 .&. 0x33333333) <<<  2) .|. ((x1 .&. 0xCCCCCCCC) >>>  2)
        x3 = ((x2 .&. 0x0F0F0F0F) <<<  4) .|. ((x2 .&. 0xF0F0F0F0) >>>  4)
        x4 = ((x3 .&. 0x00FF00FF) <<<  8) .|. ((x3 .&. 0xFF00FF00) >>>  8)
        x5 = ((x4 .&. 0x0000FFFF) <<< 16) .|. ((x4 .&. 0xFFFF0000) >>> 16)
