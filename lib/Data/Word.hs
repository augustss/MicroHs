-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(module Data.Word, Word) where
import Primitives
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
import Text.Show

instance Num Word where
  (+)  = primWordAdd
  (-)  = primWordSub
  (*)  = primWordMul
  abs x = x
  signum x = if x == 0 then 0 else 1
  fromInteger x = intToWord (_integerToInt x)

instance Integral Word where
  quot = primWordQuot
  rem  = primWordRem
  toInteger x = _intToInteger (wordToInt x)

{-
instance Bounded Word where
  minBound = 0
  maxBound = 18446744073709551615  -- 2^64-1
-}

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
  toEnum = intToWord
  fromEnum = wordToInt

intToWord :: Int -> Word
intToWord = primUnsafeCoerce

wordToInt :: Word -> Int
wordToInt = primUnsafeCoerce

--------------------------------

instance Show Word where
  show = showWord
    where
      showWord :: Word -> String
      showWord n =
        let
          c = chr ((ord '0') + (wordToInt (rem n (intToWord 10))))
        in  case n < intToWord 10 of
              False -> showWord (quot n (intToWord 10)) ++ [c]
              True  -> [c]
