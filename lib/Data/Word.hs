-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(module Data.Word, Word) where
import Primitives
import Data.Bool_Type
import Data.Bounded
import Data.Char
import Data.Eq
import Data.Int()  -- insances only
import Data.Integral
import Data.List
import Data.Num
import Text.Show

instance Num Word where
  (+)  = primWordAdd
  (-)  = primWordSub
  (*)  = primWordMul
  abs x = x
  signum x = if x == fromInt 0 then fromInt 0 else fromInt 1
  fromInt = primUnsafeCoerce

instance Integral Word where
  quot = primWordQuot
  rem  = primWordRem

{-
instance Bounded Word where
  minBound = 0
  maxBound = 18446744073709551615  -- 2^64-1
-}

--------------------------------

--infix 4 ==,/=
infix 4 <,<=,>,>=

{-
-- Comparison
(==) :: Word -> Word -> Bool
(==) = primWordEQ
(/=) :: Word -> Word -> Bool
(/=) = primWordNE
-}

instance Eq Word where
  (==) = primWordEQ
  (/=) = primWordNE

(<)  :: Word -> Word -> Bool
(<)  = primWordLT
(<=) :: Word -> Word -> Bool
(<=) = primWordLE
(>)  :: Word -> Word -> Bool
(>)  = primWordGT
(>=) :: Word -> Word -> Bool
(>=) = primWordGE

eqWord :: Word -> Word -> Bool
eqWord = (==)

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
          c = chr ((+) (ord '0') (wordToInt (rem n (intToWord 10))))
        in  case n < intToWord 10 of
              False -> showWord (quot n (intToWord 10)) ++ [c]
              True  -> [c]
