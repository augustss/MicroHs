-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Int(module Data.Int) where
import Primitives

type Int = Primitives.Int

--Yinfixl 6 +,-
--Yinfixl 7 *

-- Arithmetic
(+)  = primIntAdd
(-)  = primIntSub
(*)  = primIntMul
quot = primIntQuot
rem  = primIntRem

subtract = primIntSubR
negate x = 0 - x

--------------------------------

--Yinfix 4 ==,/=,<,<=,>,>=

-- Comparison
(==) = primIntEQ
(/=) = primIntNE

(<)  = primIntLT
(<=) = primIntLE
(>)  = primIntGT
(>=) = primIntGE

--eqInt :: Int -> Int -> Bool
eqInt = (==)

--------------------------------
