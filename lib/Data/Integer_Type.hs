-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer_Type(module Data.Integer_Type) where
import Primitives
--Yimport PrimFromInteger
import Data.List_Type

data Integer = I Sign [Digit]

data Sign = Plus | Minus

type Digit = Int

maxD :: Digit
maxD = 2147483648  -- 2^31, this is used so multiplication of two digit doesn't overflow a 64 bit Int

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

_integerToDouble :: Integer -> Double
_integerToDouble (I sign ds) = s `primDoubleMul` loop ds
  where
    loop [] = 0.0::Double
    loop (i : is) = primDoubleFromInt i `primDoubleAdd` (primDoubleFromInt maxD `primDoubleMul` loop is)
    s =
      case sign of
        Plus  -> 1.0::Double
        Minus -> 0.0 `primDoubleSub` 1.0
