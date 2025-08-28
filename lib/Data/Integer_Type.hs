-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer_Type(module Data.Integer_Type) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import {-# SOURCE #-} Control.Error
import Data.Bool_Type
import Data.List_Type

--
-- The Integer is stored in sign-magnitude format with digits in base maxD (2^32)
-- It has the following invariants:
--  * each digit is >= 0 and < maxD
--  * least significant digits first, most significant last
--  * no trailing 0s in the digits
--  * 0 is positive

data Integer = I Sign [Digit] deriving ()

data Sign = Plus | Minus deriving ()

type Digit = Word

maxD :: Digit
maxD = 1 `primWordShl` shiftD

shiftD :: Int
shiftD = _wordSize `primIntShr` 1 -- this is used so multiplication of two digits doesn't overflow a Word

quotMaxD :: Digit -> Digit
quotMaxD d = d `primWordShr` shiftD

remMaxD :: Digit -> Digit
remMaxD d = d `primWordAnd` (maxD `primWordSub` 1)

-- Sadly, we also need a bunch of functions.

_intToInteger :: Int -> Integer
_intToInteger i
  | i `primIntEQ` 0 = I Plus []
  | i `primIntGE` 0 = f Plus (primIntToWord i)
  | True            = f Minus (primIntToWord (0 `primIntSub` i))
  where
    f sign i =
      let
        high = quotMaxD i
        low = remMaxD i
      in if high `primWordEQ` 0 then I sign [low] else I sign [low, high]

_integerToInt :: Integer -> Int
_integerToInt x = primWordToInt (_integerToWord x)

_wordToInteger :: Word -> Integer
_wordToInteger i
  | i    `primWordEQ` 0 = I Plus []
  | high `primWordEQ` 0 = I Plus [low]
  | True                = I Plus [low, high]
  where
    high = quotMaxD i
    low = remMaxD i

_integerToWord :: Integer -> Word
_integerToWord (I sign ds) =
  case sign of
    Plus  -> i
    Minus -> 0 `primWordSub` i
  where
    i =
      case ds of
        []          -> 0 :: Word
        [d1]        -> d1
        d1 : d2 : _ -> d1 `primWordAdd` (d2 `primWordShl` shiftD)

_integerToWord64 :: Integer -> Word64
_integerToWord64 (I sign ds) =
  case sign of
    Plus  -> i
    Minus -> 0 `primWord64Sub` i
  where
    xadd = primWord64Add
    xmul x = primWord64Shl x shiftD
    conv [] = []
    conv (x:xs) = primWordToWord64 x : conv xs
    i =
      case conv ds of
        []            -> 0 :: Word64
        [d1]          -> d1
        -- XXX should really depend of the width of a digit
        [d1, d2]      -> xmul                                  d2  `xadd` d1
        [d1, d2, d3]  -> xmul (xmul                 d3  `xadd` d2) `xadd` d1
        d1:d2:d3:d4:_ -> xmul (xmul (xmul d4 `xadd` d3) `xadd` d2) `xadd` d1

_word64ToInteger :: Word64 -> Integer
_word64ToInteger w = I Plus (conv w)
  where conv i | primWord64EQ i 0 = []
               | True = primWord64ToWord (primWord64Rem i base) : conv (primWord64Quot i base)
        base = primWordToWord64 maxD

_integerToInt64 :: Integer -> Int64
_integerToInt64 x = primWord64ToInt64 (_integerToWord64 x)

_int64ToInteger :: Int64 -> Integer
_int64ToInteger i
  | i `primInt64EQ` 0 = I Plus []
  | i `primInt64GE` 0 = _word64ToInteger (primInt64ToWord64 i)
  | True              = I Minus ds
    where I _ ds = _word64ToInteger (primInt64ToWord64 (0 `primInt64Sub` i))

_integerToFloat :: Integer -> Float
_integerToFloat (I sign ds) = sgn (loop ds)
  where
    loop [] = 0.0 :: Float
    loop (d : ds) = primFloatFromInt (primWordToInt d) `primFloatAdd` (primFloatFromInt (primWordToInt maxD) `primFloatMul` loop ds)
    sgn x =
      case sign of
        Plus  -> x
        Minus -> primFloatNeg x

_integerToDouble :: Integer -> Double
_integerToDouble (I sign ds) = sgn (loop ds)
  where
    loop [] = 0.0 :: Double
    loop (d : ds) = primDoubleFromInt (primWordToInt d) `primDoubleAdd` (primDoubleFromInt (primWordToInt maxD) `primDoubleMul` loop ds)
    sgn x =
      case sign of
        Plus  -> x
        Minus -> primDoubleNeg x
