-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer_Type(module Data.Integer_Type) where
import Prelude()              -- do not import Prelude
import Primitives
import {-# SOURCE #-} Control.Error
import Data.Bool_Type
import Data.List_Type

data Integer = I Sign [Digit]

data Sign = Plus | Minus

type Digit = Int

maxD :: Digit
maxD =
  if _wordSize `primIntEQ` 64 then
    (2147483648::Int)  -- 2^31, this is used so multiplication of two digits doesn't overflow a 64 bit Int
  else if _wordSize `primIntEQ` 32 then
    (32768::Int)       -- 2^15, this is used so multiplication of two digits doesn't overflow a 32 bit Int
  else
    error "Integer: unsupported word size"

-- Sadly, we also need a bunch of functions.

_intToInteger :: Int -> Integer
_intToInteger i | i `primIntGE` 0  = I Plus  (f i)
                | i `primIntEQ` ni = I Minus [0::Int,0::Int,2::Int]  -- we are at minBound::Int.
                | True             = I Minus (f ni)
  where
    ni = (0::Int) `primIntSub` i
    f :: Int -> [Int]
    f x = if primIntEQ x (0::Int) then [] else primIntRem x maxD : f (primIntQuot x maxD)

_integerToInt :: Integer -> Int
_integerToInt (I sign ds) = s `primIntMul` i
  where
    i =
      case ds of
        []         -> 0::Int
        [d1]       -> d1
        [d1,d2]    -> d1 `primIntAdd` (maxD `primIntMul` d2)
        d1:d2:d3:_ -> d1 `primIntAdd` (maxD `primIntMul` (d2 `primIntAdd` (maxD `primIntMul` d3)))
    s =
      case sign of
        Plus  -> 1::Int
        Minus -> 0 `primIntSub` 1

_wordToInteger :: Word -> Integer
_wordToInteger i = I Plus  (f i)
  where
    f :: Word -> [Int]
    f x = if x `primWordEQ` (0::Word) then [] else primWordToInt (primWordRem x (primIntToWord maxD)) : f (primWordQuot x (primIntToWord maxD))

_integerToWord :: Integer -> Word
_integerToWord x = primIntToWord (_integerToInt x)

_integerToFloatW :: Integer -> FloatW
_integerToFloatW (I sign ds) = s `primFloatWMul` loop ds
  where
    loop [] = 0.0::FloatW
    loop (i : is) = primFloatWFromInt i `primFloatWAdd` (primFloatWFromInt maxD `primFloatWMul` loop is)
    s =
      case sign of
        Plus  -> 1.0::FloatW
        Minus -> 0.0 `primFloatWSub` 1.0
