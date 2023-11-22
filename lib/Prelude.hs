-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Prelude(
  module Control.Applicative,
  module Control.Error,
  module Control.Monad,
  module Data.Bool,
  module Data.Bounded,
  module Data.Char,
  module Data.Double,
  module Data.Either,
  module Data.Enum,
  module Data.Eq,
  module Data.Floating,
  module Data.Fractional,
  module Data.Function,
  module Data.Functor,
  module Data.Int,
  module Data.Integer,
  module Data.Integral,
  module Data.List,
  module Data.Maybe,
  module Data.Num,
  module Data.Ord,
  module Data.Ratio,
  module Data.Real,
  module Data.RealFloat,
  module Data.Tuple,
  module System.IO,
  module Text.Show,
  module Text.String,
  usingMhs,
  ) where
import Control.Applicative
import Control.Error
import Control.Monad
import Data.Bool
import Data.Bounded
import Data.Char
import Data.Double
import Data.Either
import Data.Enum
import Data.Eq
import Data.Floating
import Data.Fractional
import Data.Function
import Data.Functor
import Data.Int
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe
import Data.Num
import Data.Ord
import Data.Ratio(Rational)
import Data.Real
import Data.RealFloat
import Data.Tuple
import System.IO
import Text.Show
import Text.String

-- So we can detect mhs vs ghc
usingMhs :: Bool
usingMhs = True
