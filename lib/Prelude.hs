-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Prelude(
  module Control.Applicative,
  module Control.Error,
  module Control.Monad,
  module Data.Bool,
  module Data.Char,
  module Data.Either,
  module Data.Eq,
  module Data.Function,
  module Data.Functor,
  module Data.Int,
  module Data.Integral,
  module Data.List,
  module Data.Maybe,
  module Data.Num,
  module Data.Ord,
  module Data.Tuple,
  module System.IO,
  module Text.Show,
  module Text.String,
  --Ymodule Primitives,
  ) where
--Yimport Primitives(fromInteger, fromRational, ifThenElse)
import Control.Applicative
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.Integral
import Data.List
import Data.Maybe
import Data.Num
import Data.Ord
import Data.Tuple
import System.IO
import Text.Show
import Text.String

{-
-- Called on pattern match failure.
_noMatch :: forall a . [Char] -> Int -> Int -> a
_noMatch fn l c = error $ "no match at " ++
  if null fn then "no location" else
  showString fn ++ ": " ++ "line " ++ showInt l ++ ", col " ++ showInt c
-}
