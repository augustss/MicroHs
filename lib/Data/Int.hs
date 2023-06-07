module Data.Int(module Data.Int) where

-- Arithmetic
(+)  = primitive "+"
(-)  = primitive "-"
(*)  = primitive "*"
quot = primitive "quot"
rem  = primitive "rem"

subtract = primitive "subtract"
negate x = 0 - x

--------------------------------

-- Comparison
(==) = primitive "=="
(/=) = primitive "/="

(<)  = primitive "<"
(<=) = primitive "<="
(>)  = primitive ">"
(>=) = primitive ">="

--------------------------------

-- Conversion
chr x = x
ord x = x
