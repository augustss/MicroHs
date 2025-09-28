module MiniPrelude(
  module Control.Applicative,
  module Control.Error,
  module Control.Monad,
  module Data.Bool,
  module Data.Char,
  module Data.Enum,
  module Data.Eq,
  module Data.Either,
  module Data.Function,
  module Data.Functor,
  module Data.Ord,
  module Data.Int.Int,
  module Data.Integral,
  module Data.List,
  module Data.Maybe,
  module Data.Monoid.Internal,
  module Data.Num,
  module Data.Records,
  module Data.String,
  module Data.Tuple,
  module Data.Typeable,
  module System.IO.Base,
  module Text.Show,
  _usingMhs, _wordSize, _isWindows,
  ) where
import qualified Prelude()
import Control.Applicative(Applicative(..))
import Control.Error
import Control.Monad
import Data.Bool
import Data.Char
import Data.Enum
import Data.Eq
import Data.Either
import Data.Function
import Data.Functor
import Data.Ord
import Data.Int.Int
import Data.Integral
import Data.List([](..), map, (++), filter, head, last, tail, init, null, length, (!!),
                 reverse, foldl, foldl1, foldr, foldr1, and, or, any, all,
                 sum, product, concat, concatMap, maximum, minimum,
                 scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle,
                 take, drop, splitAt, takeWhile, dropWhile, span, break,
                 elem, notElem, lookup, zip, zip3, zipWith, zipWith3, unzip, unzip3,
                 lines, words, unlines, unwords)
import Data.Maybe
import Data.Monoid.Internal
import Data.Num
import Data.Records
import Data.String
import Data.Tuple
import {-# SOURCE #-} Data.Typeable
import System.IO.Base(IO, putChar, putStr, putStrLn, print, getLine, getContents, interact,
                      FilePath, readFile, writeFile, appendFile,
                      cprint, cuprint)
import Text.Show(Show(..), ShowS, shows, showChar, showString, showParen)
import Text.Show
import Primitives(_wordSize, _isWindows)

-- So we can detect mhs vs ghc
_usingMhs :: Bool
_usingMhs = True
