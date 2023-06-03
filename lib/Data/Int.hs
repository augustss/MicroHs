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
chr  = primitive "chr"
ord  = primitive "ord"
