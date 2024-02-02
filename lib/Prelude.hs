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
  module Data.Monoid,
  module Data.Num,
  module Data.Ord,
  module Data.Proxy,
  module Data.Ratio,
  module Data.Real,
  module Data.RealFloat,
  module Data.Records,
  module Data.Tuple,
  module System.IO,
  module Text.Show,
  module Text.String,
  usingMhs, _wordSize, _isWindows,
  ) where
import Control.Applicative(Applicative(..))
import Control.Error(error, undefined)
import Control.Monad(Monad(..), MonadFail(..), mapM, mapM_, sequence, sequence_, (=<<))
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bounded(Bounded(..))
import Data.Char(Char, String)
import Data.Double(Double)
import Data.Either(Either(..), either)
import Data.Enum(Enum(..))
import Data.Eq(Eq(..))
import Data.Floating(Floating(..))
import Data.Fractional(Fractional(..))
import Data.Function(id, const, (.), flip, ($), seq, ($!), until, curry, uncurry)
import Data.Functor
import Data.Int
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe(Maybe(..), maybe)
import Data.Monoid
import Data.Num
import Data.Ord(Ord(..), Ordering(..))
import Data.Proxy
import Data.Ratio(Rational)
import Data.Real(Real(..))
import Data.RealFloat
import Data.Records
import Data.Semigroup((<>))
import Data.Tuple(()(..), fst, snd)
import System.IO
import Text.Show
import Text.String
import Primitives(_wordSize, _isWindows)

-- So we can detect mhs vs ghc
usingMhs :: Bool
usingMhs = True
