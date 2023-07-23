-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Int(module Data.Int) where
import Primitives
import Data.Bool_Type

type Int = Primitives.Int

--Yinfixl 6 +,-
--Yinfixl 7 *

-- Arithmetic
(+) :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
(+)  = primIntAdd
(-) :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
(-)  = primIntSub
(*) :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
(*)  = primIntMul
quot :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
quot = primIntQuot
rem :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
rem  = primIntRem

subtract :: Data.Int.Int -> Data.Int.Int -> Data.Int.Int
subtract = primIntSubR

negate :: Data.Int.Int -> Data.Int.Int
negate x = 0 - x

--------------------------------

--Yinfix 4 ==,/=,<,<=,>,>=

-- Comparison
(==) :: Data.Int.Int -> Data.Int.Int -> Bool
(==) = primIntEQ
(/=) :: Data.Int.Int -> Data.Int.Int -> Bool
(/=) = primIntNE

(<)  :: Data.Int.Int -> Data.Int.Int -> Bool
(<)  = primIntLT
(<=) :: Data.Int.Int -> Data.Int.Int -> Bool
(<=) = primIntLE
(>)  :: Data.Int.Int -> Data.Int.Int -> Bool
(>)  = primIntGT
(>=) :: Data.Int.Int -> Data.Int.Int -> Bool
(>=) = primIntGE

eqInt :: Data.Int.Int -> Data.Int.Int -> Bool
eqInt = (==)

--------------------------------
