-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Bool(
  module Data.Bool,
  module Data.Bool_Type
  ) where
import Primitives
import Data.Bool_Type
import Data.Eq
import Text.Show

instance Eq Bool where
  False == x  =  not x
  True  == x  =  x

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

eqBool :: Bool -> Bool -> Bool
eqBool True  x = x
eqBool False x = not x

neBool :: Bool -> Bool -> Bool
neBool True  x = not x
neBool False x = x

instance Show Bool where
  showsPrec _ False = showString "False"
  showsPrec _ True  = showString "True"
