module Data.List_Type(module Data.List_Type) where
import Primitives

infixr 5 :
data [] a = [] | (:) a [a]  -- Parser hacks makes this acceptable

-- This does not really belong here, but it makes the module structure
-- much simpler.
infixr 5 ++
(++) :: forall a . [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : xs ++ ys 
