-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Prelude(
  module Control.Applicative,
  module Control.Error,
  module Control.Monad,
  module Control.Monad.Fail,
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
  module Data.Ratio,
  module Data.Real,
  module Data.RealFloat,
  module Data.RealFrac,
  module Data.Records,
  module Data.Semigroup,
  module Data.Tuple,
  module Data.Word,
  module System.IO,
  module Text.Read,
  module Text.Show,
  module Text.String,
  Float,
  usingMhs, _wordSize, _isWindows,
  ) where
import Prelude()              -- do not import Prelude
import Control.Applicative(Applicative(..))
import Control.Error(error, undefined)
import Control.Monad(Monad(..), mapM, mapM_, sequence, sequence_, (=<<))
import Control.Monad.Fail(MonadFail(..))
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bounded(Bounded(..))
import Data.Char(Char, String)
import Data.Double(Double)
import Data.Either(Either(..), either)
import Data.Enum(Enum(..))
import Data.Eq(Eq(..))
import Data.Floating(Floating(..))
import Data.Fractional(Fractional(..), (^^))
import Data.Function(id, const, (.), flip, ($), seq, ($!), until, curry, uncurry)
import Data.Functor(Functor(..), (<$>))
import Data.Int(Int)
import Data.Integer(Integer)
import Data.Integral(Integral(..), fromIntegral, gcd, lcm, even, odd, (^))
import Data.List
import Data.Maybe(Maybe(..), maybe)
import Data.Monoid(Monoid(..))
import Data.Num(Num(..), subtract)
import Data.Ord(Ord(..), Ordering(..))
import Data.Ratio(Rational)
import Data.Real(Real(..), realToFrac)
import Data.RealFloat(RealFloat(..))
import Data.RealFrac(RealFrac(..))
import Data.Records  -- XXX redo this somehow
import Data.Semigroup(Semigroup(..))
import Data.Tuple(()(..), fst, snd)
import Data.Word(Word)
import System.IO(IO, putChar, putStr, putStrLn, print, getLine, getContents, interact,
                 FilePath, readFile, writeFile, appendFile,
                 cprint)
import Text.Read(ReadS, Read(..), read, reads, readParen, lex)
import Text.Show(Show(..), ShowS, shows, showChar, showString, showParen)
import Text.String
import Primitives(_wordSize, _isWindows)

-- So we can detect mhs vs ghc
usingMhs :: Bool
usingMhs = True

type Float = Double
