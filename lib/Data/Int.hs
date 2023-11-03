-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Int(module Data.Int, Int) where
import Primitives
import Data.Bool_Type
import Data.Bounded
import Data.Char_Type
import Data.Eq
import Data.Integer_Type
import Data.Integral
import Data.List_Type
import Data.Num
import Data.Ord
import Text.Show

instance Num Int where
  (+)  = primIntAdd
  (-)  = primIntSub
  (*)  = primIntMul
  negate x = primIntSub 0 x
  abs x = if x < 0 then negate x else x
  signum x =
    case compare x 0 of
      LT -> -1
      EQ ->  0
      GT ->  1
  fromInteger = _integerToInt

instance Integral Int where
  quot = primIntQuot
  rem  = primIntRem

{-
instance Bounded Int where
  minBound = -9223372036854775808   -- -2^63
  maxBound =  9223372036854775807   --  2^63-1
-}

--------------------------------

-- infix 4 ==,/=
--infix 4 <,<=,>,>=

-- Comparison
{-
(==) :: Int -> Int -> Bool
(==) = primIntEQ
(/=) :: Int -> Int -> Bool
(/=) = primIntNE
-}
instance Eq Int where
  (==) = primIntEQ
  (/=) = primIntNE

{-
(<)  :: Int -> Int -> Bool
(<)  = primIntLT
(<=) :: Int -> Int -> Bool
(<=) = primIntLE
(>)  :: Int -> Int -> Bool
(>)  = primIntGT
(>=) :: Int -> Int -> Bool
(>=) = primIntGE
-}
instance Ord Int where
  (<)  = primIntLT
  (<=) = primIntLE
  (>)  = primIntGT
  (>=) = primIntGE

--------------------------------

instance Show Int where
  show = showInt_

-- XXX these should not be exported
-- XXX wrong for minInt
showInt_ :: Int -> String
showInt_ n =
  if n < 0 then
    '-' : showUnsignedInt_ (negate n)
  else
    showUnsignedInt_ n

showUnsignedInt_ :: Int -> String
showUnsignedInt_ n =
  let
    c = primChr (primOrd '0' + rem n 10)
  in  if n < 10 then
        [c]
      else
        showUnsignedInt_ (quot n 10) ++ [c]
