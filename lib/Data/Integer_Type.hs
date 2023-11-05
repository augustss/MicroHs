-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer_Type(module Data.Integer_Type) where
import Primitives
import Data.Bool_Type
--Yimport PrimFromInteger
import Data.List_Type

data Integer = I Sign [Digit]

data Sign = Plus | Minus

type Digit = Int

maxD :: Digit
maxD = 2147483648  -- 2^31, this is used so multiplication of two digit doesn't overflow a 64 bit Int

_intToInteger :: Int -> Integer
_intToInteger i | i `primIntGE` 0  = I Plus  (f i)
                | i `primIntEQ` ni = I Minus [0::Int,0::Int,2::Int]  -- we are at minBound::Int.  XXX deal with this in a more portable way.
                | True             = I Minus (f ni)
  where
    ni = (0::Int) `primIntSub` i
    f :: Int -> [Int]
    f 0 = []
    f x = primIntRem x maxD : f (primIntQuot x maxD)

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

_integerToDouble :: Integer -> Double
_integerToDouble (I sign ds) = s `primDoubleMul` loop ds
  where
    loop [] = 0.0::Double
    loop (i : is) = primDoubleFromInt i `primDoubleAdd` (primDoubleFromInt maxD `primDoubleMul` loop is)
    s =
      case sign of
        Plus  -> 1.0::Double
        Minus -> 0.0 `primDoubleSub` 1.0
