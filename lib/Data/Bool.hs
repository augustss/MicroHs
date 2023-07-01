module Data.Bool(
  module Data.Bool,
  module Data.Bool_Type
  ) where
import Data.Bool_Type

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

