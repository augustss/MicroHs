module Data.Integer_Type(module Data.Integer_Type) where
import Primitives

data Integer = I Sign [{-Digit-}Int]

data Sign = Plus | Minus
