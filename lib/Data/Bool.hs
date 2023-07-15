-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Bool(
  module Data.Bool,
  module Data.Bool_Type
  ) where
import Data.Bool_Type

--Yinfixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) x y =
  case x of
    False -> y
    True  -> True

--Yinfixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) x y =
  case x of
    False -> False
    True  -> y

not :: Bool -> Bool
not b =
  case b of
    False -> True
    True  -> False
