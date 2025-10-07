-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word.Word8(Word8, bitReverse8, intToWord8, word8ToInt) where
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

newtype Word8 = W8 Word
  deriving (Typeable, Eq, Ord)

unW8 :: Word8 -> Word
unW8 (W8 x) = x

w8 :: Word -> Word8
w8 w = W8 (w .&. 0xff)

bin8 :: (Word -> Word -> Word) -> (Word8 -> Word8 -> Word8)
bin8 op (W8 x) (W8 y) = w8 (x `op` y)

bini8 :: (Word -> Int -> Word) -> (Word8 -> Int -> Word8)
bini8 op (W8 x) y = w8 (x `op` y)

una8 :: (Word -> Word) -> (Word8 -> Word8)
una8 op (W8 x) = w8 (op x)

instance Num Word8 where
  (+)  = bin8 primWordAdd
  (-)  = bin8 primWordSub
  (*)  = bin8 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w8 (_integerToWord i)

instance Integral Word8 where
  quot = bin8 primWordQuot
  rem  = bin8 primWordRem
  toInteger = _wordToInteger . unW8

instance Bounded Word8 where
  minBound = w8 0
  maxBound = w8 0xff

instance Real Word8 where
  toRational = _integerToRational . _wordToInteger . unW8

instance Show Word8 where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Word8 where
  readsPrec = readIntegral
-}

instance Enum Word8 where
  succ x = if x == maxBound then error "Word8.succ: overflow" else x + 1
  pred x = if x == minBound then error "Word8.pred: underflow" else x - 1
  toEnum = w8 . primIntToWord
  fromEnum = primWordToInt . unW8
  enumFrom n = enumFromTo n maxBound
  enumFromThen n m
    | m >= n = enumFromThenTo n m maxBound
    | otherwise = enumFromThenTo n m minBound
  enumFromTo = coerce (enumFromTo @Word)
  enumFromThenTo = coerce (enumFromThenTo @Word)

instance Bits Word8 where
  (.&.) = bin8 primWordAnd
  (.|.) = bin8 primWordOr
  xor   = bin8 primWordXor
  complement = una8 primWordInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= 8 = 0
    | otherwise = x `unsafeShiftL` i
  unsafeShiftL = bini8 primWordShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= 8 = 0
    | otherwise = x `unsafeShiftR` i
  unsafeShiftR = bini8 primWordShr
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  bit = bitDefault
  testBit = testBitDefault
  popCount = primWordPopcount . unW8
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Word8 where
  finiteBitSize _ = 8
  countLeadingZeros (W8 x) = primWordClz x - (_wordSize - 8)
  countTrailingZeros (W8 x) = if x == 0 then 8 else primWordCtz x

bitReverse8 :: Word8 -> Word8
bitReverse8 x0 = x3
  where (<<<) = unsafeShiftL
        (>>>) = unsafeShiftR
        x1 = ((x0 .&. 0x55) <<<  1) .|. ((x0 .&. 0xAA) >>>  1)
        x2 = ((x1 .&. 0x33) <<<  2) .|. ((x1 .&. 0xCC) >>>  2)
        x3 = ((x2 .&. 0x0F) <<<  4) .|. ((x2 .&. 0xF0) >>>  4)

intToWord8 :: Int -> Word8
intToWord8 i = primUnsafeCoerce (i .&. 0xFF) -- Safety: Int and Word8 have the same representation and `i .&. 0xFF` ensures that the value is in the Word8 range

word8ToInt :: Word8 -> Int
word8ToInt w = primUnsafeCoerce w -- Safety: Int and Word8 have the same representation and a Word8 is in the Int range
