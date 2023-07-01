module Data.Int(module Data.Int) where
import Primitives

type Int = Primitives.Int

-- Arithmetic
(+)  = primIntAdd
(-)  = primIntSub
(*)  = primIntMul
quot = primIntQuot
rem  = primIntRem

subtract = primIntSubR
negate x = 0 - x

--------------------------------

-- Comparison
(==) = primIntEQ
(/=) = primIntNE

(<)  = primIntLT
(<=) = primIntLE
(>)  = primIntGT
(>=) = primIntGE

--------------------------------

-- Conversion
chr = primChr
ord = primOrd
