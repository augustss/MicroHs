module MHSPrelude(
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
  module Data.Records,
  module Data.String,
  module Data.Tuple,
  module System.IO,
  module Text.Show,
  usingMhs, _wordSize, _isWindows,
  ) where
import Prelude()
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
import Data.Fractional(Fractional(..), (^^), realToFrac)
import Data.Function(id, const, (.), flip, ($), seq, ($!), until, asTypeOf)
import Data.Functor(Functor(..), (<$>))
import Data.Int(Int)
import Data.Int.Instances
import Data.Integer(Integer)
import Data.Integral(Integral(..), fromIntegral, gcd, lcm, even, odd, (^))
import Data.List([](..), map, (++), filter, head, last, tail, init, null, length, (!!),
                 reverse, foldl, foldl1, foldr, foldr1, and, or, any, all,
                 sum, product, concat, concatMap, maximum, minimum,
                 scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle,
                 take, drop, splitAt, takeWhile, dropWhile, span, break,
                 elem, notElem, lookup, zip, zip3, zipWith, zipWith3, unzip, unzip3,
                 lines, words, unlines, unwords)
import Data.Maybe(Maybe(..), maybe)
import Data.Monoid(Monoid(..))
import Data.Num(Num(..), subtract)
import Data.Ord(Ord(..), Ordering(..))
import Data.Ratio(Rational)
import Data.Real(Real(..))
import Data.Records  -- needed for data types with fields
import Data.String(IsString(..), lines, unlines, words, unwords)
import Data.Tuple(fst, snd, curry, uncurry)
import Data.Word(Word)
import System.IO(IO, putChar, putStr, putStrLn, print, getLine, getContents, interact,
                 FilePath, readFile, writeFile, appendFile,
                 cprint, cuprint)
import Text.Show(Show(..), ShowS, shows, showChar, showString, showParen)
import Primitives(_wordSize, _isWindows)

-- So we can detect mhs vs ghc
usingMhs :: Bool
usingMhs = True
