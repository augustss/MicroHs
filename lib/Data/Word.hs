-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Word(module Data.Word) where
import Primitives
import Data.Bool_Type
import qualified Data.Char as C
import qualified Data.Int as I
import Data.List
import Text.String

--type Word = Primitives.Word

--Yinfixl 6 +,-
--Yinfixl 7 *

-- Arithmetic
(+) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> {-Data.Word.-}Word
(+)  = primWordAdd
(-) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> {-Data.Word.-}Word
(-)  = primWordSub
(*) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> {-Data.Word.-}Word
(*)  = primWordMul
quot :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> {-Data.Word.-}Word
quot = primWordQuot
rem :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> {-Data.Word.-}Word
rem  = primWordRem

--------------------------------

--Yinfix 4 ==,/=,<,<=,>,>=

-- Comparison
(==) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(==) = primWordEQ
(/=) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(/=) = primWordNE

(<)  :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(<)  = primWordLT
(<=) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(<=) = primWordLE
(>)  :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(>)  = primWordGT
(>=) :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
(>=) = primWordGE

eqWord :: {-Data.Word.-}Word -> {-Data.Word.-}Word -> Bool
eqWord = (==)

intToWord :: Int -> Word
intToWord = primUnsafeCoerce

wordToInt :: Word -> Int
wordToInt = primUnsafeCoerce

--------------------------------

showWord :: Word -> C.String
showWord n =
  let
    c = C.chr ((I.+) (C.ord '0') (wordToInt (rem n (intToWord 10))))
  in  case n < intToWord 10 of
        False -> showWord (quot n (intToWord 10)) ++ [c]
        True  -> [c]
