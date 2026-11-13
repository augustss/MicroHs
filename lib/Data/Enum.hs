module Data.Enum (
  Enum(..),
  boundedEnumFrom,
  boundedEnumFromThen,
  numericEnumFrom,
  numericEnumFromThen,
  numericEnumFromTo,
  numericEnumFromThenTo,
  fractionalEnumFrom,
  fractionalEnumFromThen,
  fractionalEnumFromTo,
  fractionalEnumFromThenTo,
) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Char_Type
import Data.Bounded
import Data.Enum_Class
import Data.Fractional
import Data.Function
import Data.Functor
import Data.Int.Int
import Data.List
import Data.Num
import Data.Ord
import Data.Tuple

boundedEnumFrom :: forall a . (Enum a, Bounded a) => a -> [a]
boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

boundedEnumFromThen :: forall a . (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen n1 n2
  | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
  | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
  where
    i_n1 = fromEnum n1
    i_n2 = fromEnum n2

numericEnumFrom :: (Num a) => a -> [a]
numericEnumFrom !n = n : numericEnumFrom (n + 1)

numericEnumFromThen :: (Num a) => a -> a -> [a]
numericEnumFromThen n m = from n
  where
    d = m - n
    from !i = i : from (i + d)

numericEnumFromTo :: (Num a, Ord a) => a -> a -> [a]
numericEnumFromTo l h = takeWhile (<= h) (numericEnumFrom l)

numericEnumFromThenTo :: (Num a, Ord a) => a -> a -> a -> [a]
numericEnumFromThenTo l m h =
  if m > l then
    takeWhile (<= h) (numericEnumFromThen l m)
  else
    takeWhile (>= h) (numericEnumFromThen l m)

-- Like GHC, compute n + k*step to avoid adding up rounding errors
fractionalEnumFrom :: (Fractional a) => a -> [a]
fractionalEnumFrom n = from 0
  where
    from !k = let x = n + k in x `seq` (x : from (k + 1))

fractionalEnumFromThen :: (Fractional a) => a -> a -> [a]
fractionalEnumFromThen n m = from 0
  where
    d = m - n
    from !k = let x = n + k * d in x `seq` (x : from (k + 1))

-- For Fractional types the Report says to go up to h + step/2
fractionalEnumFromTo :: (Fractional a, Ord a) => a -> a -> [a]
fractionalEnumFromTo l h = takeWhile (<= h + 1/2) (fractionalEnumFrom l)

fractionalEnumFromThenTo :: (Fractional a, Ord a) => a -> a -> a -> [a]
fractionalEnumFromThenTo l m h =
  if m >= l then
    takeWhile (<= h + d) (fractionalEnumFromThen l m)
  else
    takeWhile (>= h + d) (fractionalEnumFromThen l m)
  where d = (m - l) / 2

-- Likewise for Bool
instance Enum Bool where
  fromEnum False = 0
  fromEnum True  = 1
  toEnum i
    | i `primIntEQ` 0 = False
    | i `primIntEQ` 1 = True
    | otherwise       = error "Enum.Bool.toEnum: bad arg"
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Enum Char where
  fromEnum = primOrd
  toEnum   = primChr
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Enum Ordering where
  fromEnum LT = 0
  fromEnum EQ = 1
  fromEnum GT = 2
  toEnum i
    | i `primIntEQ` 0 = LT
    | i `primIntEQ` 1 = EQ
    | i `primIntEQ` 2 = GT
    | otherwise       = error "Ord.toEnum: out of range"
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

-----------------------------------

instance Enum () where
  succ _      = error "Enum.().succ: bad argument"
  pred _      = error "Enum.().pred: bad argument"

  toEnum x
    | x `primIntEQ` 0    = ()
    | otherwise = error "Enum.().toEnum: bad argument"

  fromEnum () = 0
  enumFrom ()         = [()]
  enumFromThen () ()  = let many = ():many in many
  enumFromTo () ()    = [()]
  enumFromThenTo () () () = let many = ():many in many

instance (Enum a) => Enum (Solo a) where
  succ = fmap succ
  pred = fmap pred
  toEnum = MkSolo . toEnum
  fromEnum (MkSolo x) = fromEnum x
