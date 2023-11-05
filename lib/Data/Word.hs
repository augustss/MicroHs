-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(module Data.Word, Word) where
import Primitives
import Data.Bits
import Data.Bool_Type
import Data.Bounded
import Data.Char
import Data.Enum
import Data.Eq
import Data.Int()  -- instances only
import Data.Integer
import Data.Integral
import Data.List
import Data.Num
import Data.Ord
import Data.Real
import Text.Show

instance Num Word where
  (+)  = primWordAdd
  (-)  = primWordSub
  (*)  = primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger x = primIntToWord (_integerToInt x)

instance Integral Word where
  quot = primWordQuot
  rem  = primWordRem
  toInteger = _wordToInteger

instance Bounded Word where
  minBound = 0::Word
  maxBound = (-1::Word)

instance Real Word where
  toRational i = _integerToRational (_wordToInteger i)

--------------------------------

instance Enum Word where
  succ x = x + 1
  pred x = x - 1
  toEnum = primIntToWord
  fromEnum = primWordToInt
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

instance Eq Word where
  (==) = primWordEQ
  (/=) = primWordNE

instance Ord Word where
  (<)  = primWordLT
  (<=) = primWordLE
  (>)  = primWordGT
  (>=) = primWordGE

instance Enum Word where
  toEnum = primIntToWord
  fromEnum = primWordToInt

--------------------------------

instance Bits Word where
  (.&.) = primWordAnd
  (.|.) = primWordOr
  xor   = primWordXor
  complement = primWordInv
  shiftL = primWordShl
  shiftR = primWordShr
--  bitSizeMaybe _ = Just 64   -- XXX
  bitSize _ = 64
  bit n = primWordShl 1 n
  zeroBits = 0

--------------------------------

instance Show Word where
  show = showWord
    where
      showWord :: Word -> String
      showWord n =
        let
          c = chr (ord '0' + primWordToInt (rem n (10::Word)))
        in  case n < (10::Word) of
              False -> showWord (quot n (10::Word)) ++ [c]
              True  -> [c]
