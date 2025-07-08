-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer_Type(module Data.Integer_Type) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import {-# SOURCE #-} Control.Error
import Data.Bool_Type
import Data.List_Type

--
-- The Integer is stored in sign-magnitude format with digits in base maxD (2^31)
-- It has the following invariants:
--  * each digit is >= 0 and < maxD
--  * least significant digits first, most significant last
--  * no trailing 0s in the digits
--  * 0 is positive

data Integer = I Sign [Digit]

data Sign = Plus | Minus

type Digit = Word64

maxD :: Digit
maxD = 1 `primWord64Shl` shiftD

shiftD :: Int
shiftD = 32 -- this is used so multiplication of two digits doesn't overflow a Word64

quotMaxD :: Digit -> Digit
quotMaxD d = d `primWord64Shr` shiftD

remMaxD :: Digit -> Digit
remMaxD d = d `primWord64And` (maxD `primWord64Sub` 1)

-- Sadly, we also need a bunch of functions.

_intToInteger :: Int -> Integer
_intToInteger x = _int64ToInteger (primIntToInt64 x)

_int64ToInteger :: Int64 -> Integer
_int64ToInteger i
  | i `primInt64EQ` 0 = I Plus []
  | i `primInt64GE` 0 = f Plus (primInt64ToWord64 i)
  | True              = f Minus (primInt64ToWord64 (0 `primInt64Sub` i))
  where
    f sign i =
      let
        high = quotMaxD i
        low = remMaxD i
      in if high `primWord64EQ` 0 then I sign [low] else I sign [low, high]

_integerToInt :: Integer -> Int
_integerToInt x = primInt64ToInt (_integerToInt64 x)

_integerToInt64 :: Integer -> Int64
_integerToInt64 x = primWord64ToInt64 (_integerToWord64 x)

_wordToInteger :: Word -> Integer
_wordToInteger x = _word64ToInteger (primWordToWord64 x)

_word64ToInteger :: Word64 -> Integer
_word64ToInteger i
  | i    `primWord64EQ` 0 = I Plus []
  | high `primWord64EQ` 0 = I Plus [low]
  | True                = I Plus [low, high]
  where
    high = quotMaxD i
    low = remMaxD i

_integerToWord :: Integer -> Word
_integerToWord x = primWord64ToWord (_integerToWord64 x)

_integerToWord64 :: Integer -> Word64
_integerToWord64 (I sign ds) =
  case sign of
    Plus  -> i
    Minus -> 0 `primWord64Sub` i
  where
    i =
      case ds of
        []          -> 0 :: Word64
        [d1]        -> d1
        d1 : d2 : _ -> d1 `primWord64Add` (d2 `primWord64Shl` shiftD)

_integerToFloat :: Integer -> Float
_integerToFloat (I sign ds) = sgn (loop ds)
  where
    loop [] = 0.0 :: Float
    loop (d : ds) = primFloatFromInt64 (primWord64ToInt64 d) `primFloatAdd` (primFloatFromInt64 (primWord64ToInt64 maxD) `primFloatMul` loop ds)
    sgn x =
      case sign of
        Plus  -> x
        Minus -> primFloatNeg x

_integerToDouble :: Integer -> Double
_integerToDouble (I sign ds) = sgn (loop ds)
  where
    loop [] = 0.0 :: Double
    loop (d : ds) = primDoubleFromInt64 (primWord64ToInt64 d) `primDoubleAdd` (primDoubleFromInt64 (primWord64ToInt64 maxD) `primDoubleMul` loop ds)
    sgn x =
      case sign of
        Plus  -> x
        Minus -> primDoubleNeg x
