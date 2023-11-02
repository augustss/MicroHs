-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Int(module Data.Int, Int) where
import Primitives
import Data.Bool_Type
import Data.Char_Type
import Data.Eq
import Data.List_Type
import Data.Ord
import Text.Show

infixl 6 +,-
infixl 7 *,`quot`,`rem`

-- Arithmetic
(+) :: Int -> Int -> Int
(+)  = primIntAdd
(-) :: Int -> Int -> Int
(-)  = primIntSub
(*) :: Int -> Int -> Int
(*)  = primIntMul
quot :: Int -> Int -> Int
quot = primIntQuot
rem :: Int -> Int -> Int
rem  = primIntRem

subtract :: Int -> Int -> Int
subtract = primIntSubR

negate :: Int -> Int
negate x = 0 - x

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
