module Data.Bool(module Data.Bool) where

data Bool = False | True

(||) :: Bool -> Bool -> Bool
(||) x y =
  case x of
    False -> y
    True  -> True

(&&) :: Bool -> Bool -> Bool
(&&) x y =
  case x of
    False -> False
    True  -> y

