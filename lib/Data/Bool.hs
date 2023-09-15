-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Bool(
  module Data.Bool,
  module Data.Bool_Type
  ) where
import Primitives
import Data.Bool_Type

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) False y = y
(||) True  _ = True

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) False _ = False
(&&) True  y = y

not :: Bool -> Bool
not False = True
not True  = False

otherwise :: Bool
otherwise = True
