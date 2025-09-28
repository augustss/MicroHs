-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word.Word(Word) where
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
import Numeric.Show
import Text.Show

instance Num Word where
  (+)  = primWordAdd
  (-)  = primWordSub
  (*)  = primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger = _integerToWord

instance Integral Word where
  quot = primWordQuot
  rem  = primWordRem
  toInteger = _wordToInteger

instance Bounded Word where
  minBound = 0::Word
  maxBound = primWordInv (0::Word)

instance Real Word where
  toRational i = _integerToRational (_wordToInteger i)

instance Show Word where
  showsPrec _ = showUnsigned

-- Avoid showIntegral to avoid Integer
showUnsigned :: Word -> ShowS
showUnsigned n r =
  let c = primChr (primOrd '0' + primWordToInt (rem n (10::Word)))
  in  if n < (10::Word) then
        c : r
      else
        showUnsigned (quot n (10::Word)) (c : r)

{- in Text.Read.Internal
instance Read Word where
  readsPrec = readIntegral
-}

--------------------------------

eftWord :: Word -> Word -> [Word]
eftWord x y
  | x `primWordGT` y = []
  | otherwise = go x
  where
    go n = n : if n `primWordEQ` y then [] else go (n `primWordAdd` 1)

efttWordUp :: Word -> Word -> Word -> [Word]
-- x2 >= x1
efttWordUp x1 x2 y
  | y `primWordLT` x2 = if y `primWordLT` x1 then [] else [x1]
  | otherwise =
    let
      delta = x2 `primWordSub` x1
      y' = y `primWordSub` delta
      go x = if x `primWordGT` y' then [x] else x : go (x `primWordAdd` delta)
    in x1 : go x2

efttWordDown :: Word -> Word -> Word -> [Word]
-- x2 <= x1
efttWordDown x1 x2 y
  | y `primWordGT` x2 = if y `primWordGT` x1 then [] else [x1]
  | otherwise =
    let
      delta = x2 `primWordSub` x1
      y' = y `primWordSub` delta
      go x = if x `primWordLT` y' then [x] else x : go (x `primWordAdd` delta)
    in x1 : go x2

instance Enum Word where
  succ x = if x `primWordEQ` maxBound then error "Word.succ: overflow" else x + 1
  pred x = if x `primWordEQ` minBound then error "Word.pred: underflow" else x - 1
  toEnum = primIntToWord
  fromEnum = primWordToInt
  enumFrom n = eftWord n maxBound
  enumFromThen x1 x2
    | x2 `primWordGE` x1 = efttWordUp x1 x2 maxBound
    | otherwise          = efttWordDown x1 x2 minBound
  enumFromTo = eftWord
  enumFromThenTo x1 x2 y
    | x2 `primWordGE` x1 = efttWordUp x1 x2 y
    | otherwise          = efttWordDown x1 x2 y

--------------------------------

instance Eq Word where
  (==) = primWordEQ
  (/=) = primWordNE

instance Ord Word where
  compare = primWordCompare
  (<)  = primWordLT
  (<=) = primWordLE
  (>)  = primWordGT
  (>=) = primWordGE

--------------------------------

instance Bits Word where
  (.&.) = primWordAnd
  (.|.) = primWordOr
  xor   = primWordXor
  complement = primWordInv
  x `shiftL` i
    | i < 0 = _overflowError
    | i >= _wordSize = 0
    | otherwise = x `primWordShl` i
  unsafeShiftL = primWordShl
  x `shiftR` i
    | i < 0 = _overflowError
    | i >= _wordSize = 0
    | otherwise = x `primWordShr` i
  unsafeShiftR = primWordShr
  bitSizeMaybe _ = Just _wordSize
  bitSize _ = _wordSize
  bit = bitDefault
  testBit = testBitDefault
  popCount = primWordPopcount
  zeroBits = 0
  isSigned _ = False

instance FiniteBits Word where
  finiteBitSize _ = _wordSize
  countLeadingZeros = primWordClz
  countTrailingZeros = primWordCtz

