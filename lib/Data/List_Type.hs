module Data.List_Type(module Data.List_Type) where
import Prelude()              -- do not import Prelude
import Primitives

infixr 5 :
data [] a = [] | (:) a [a]  -- Parser hacks makes this acceptable

-- This does not really belong here, but it makes the module structure
-- much simpler.
infixr 5 ++
(++) :: forall a . [a] -> [a] -> [a]
axs ++ ys =
  let rec [] = ys
      rec (x:xs) = x : rec xs
  in  rec axs

-- Put concatMap here so list comprehensions can be desugared
-- using only List_Type
concatMap :: forall a b . (a -> [b]) -> [a] -> [b]
concatMap f = rec
  where
    rec [] = []
    rec (x:xs) = f x ++ rec xs
