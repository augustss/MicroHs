module Data.List_Type(module Data.List_Type) where

infixr 5 :
data [] a = [] | (:) a [a]  -- Parser hacks makes this acceptable
