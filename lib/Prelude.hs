-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Prelude(
  module Control.Error,
  module Data.Bool,
  module Data.Char,
  module Data.Either,
  module Data.Eq,
  module Data.Function,
  module Data.Int,
  module Data.List,
  module Data.Maybe,
  module Data.Ord,
  module Data.Tuple,
  module System.IO,
  module Text.String,
  ) where
import Control.Error
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Ord
import Data.Tuple
import System.IO
import Text.String

{-
-- Called on pattern match failure.
_noMatch :: forall a . [Char] -> Int -> Int -> a
_noMatch fn l c = error $ "no match at " ++
  if null fn then "no location" else
  showString fn ++ ": " ++ "line " ++ showInt l ++ ", col " ++ showInt c
-}
