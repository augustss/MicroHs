-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word8(Word8) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bits
import Data.Bool_Type
import Data.Bounded
import Data.Char
import Data.Enum
import Data.Eq
import Data.Function
import Data.Int()  -- instances only
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Real
import Data.Word
import Numeric
import Text.Read
import Text.Show

newtype Word8 = W8 Word
unW8 :: Word8 -> Word
unW8 (W8 x) = x

w8 :: Word -> Word8
w8 w = W8 (w .&. 0xff)

bin8 :: (Word -> Word -> Word) -> (Word8 -> Word8 -> Word8)
bin8 op (W8 x) (W8 y) = w8 (x `op` y)

bini8 :: (Word -> Int -> Word) -> (Word8 -> Int -> Word8)
bini8 op (W8 x) y = w8 (x `op` y)

cmp8 :: (Word -> Word -> Bool) -> (Word8 -> Word8 -> Bool)
cmp8 op (W8 x) (W8 y) = x `op` y

una8 :: (Word -> Word) -> (Word8 -> Word8)
una8 op (W8 x) = w8 (op x)

instance Num Word8 where
  (+)  = bin8 primWordAdd
  (-)  = bin8 primWordSub
  (*)  = bin8 primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger i = w8 (primIntToWord (_integerToInt i))

instance Integral Word8 where
  quot = bin8 primWordQuot
  rem  = bin8 primWordRem
  toInteger = _wordToInteger . unW8

instance Bounded Word8 where
  minBound = W8 0
  maxBound = W8 0xff

instance Real Word8 where
  toRational = _integerToRational . _wordToInteger . unW8

instance Show Word where
  showsPrec = showIntegral

instance Read Word where
  readsPrec = readIntegral

--------------------------------

instance Enum Word8 where
  succ x = x + 1
  pred x = x - 1
  toEnum = w8 . primIntToWord
  fromEnum = primWordToInt . unW8
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

--------------------------------

instance Eq Word8 where
  (==) = cmp8 primWordEQ
  (/=) = cmp8 primWordNE

instance Ord Word8 where
  (<)  = cmp8 primWordLT
  (<=) = cmp8 primWordLE
  (>)  = cmp8 primWordGT
  (>=) = cmp8 primWordGE

--------------------------------

instance Bits Word8 where
  (.&.) = bin8 primWordAnd
  (.|.) = bin8 primWordOr
  xor   = bin8 primWordXor
  complement = una8 primWordInv
  shiftL = bini8 primWordShl
  shiftR = bini8 primWordShr
  bitSizeMaybe _ = Just 8
  bitSize _ = 8
  bit n = w8 (primWordShl 1 n)
  zeroBits = 0
