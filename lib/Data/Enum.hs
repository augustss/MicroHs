module Data.Enum (
  Enum(..),
  boundedEnumFrom,
  boundedEnumFromThen,
  numericEnumFrom,
  numericEnumFromThen,
  numericEnumFromTo,
  numericEnumFromThenTo,
) where
import qualified Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Char_Type
import Data.Bounded
import Data.Function
import Data.Int
import Data.List
import Data.Num
import Data.Ord

class Enum a where
  succ           :: a -> a
  pred           :: a -> a
  toEnum         :: Int -> a
  fromEnum       :: a -> Int

  enumFrom       :: a -> [a]
  enumFromThen   :: a -> a -> [a]
  enumFromTo     :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]

  succ                   = toEnum . (+ 1) . fromEnum
  pred                   = toEnum . (subtract 1) . fromEnum
  enumFrom x             = map toEnum [fromEnum x ..]
  enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
  enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
  enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

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
numericEnumFrom n = n : numericEnumFrom (n + 1)

numericEnumFromThen :: (Num a) => a -> a -> [a]
numericEnumFromThen n m = from n
  where
    d = m - n
    from i = i : from (i + d)

numericEnumFromTo :: (Num a, Ord a) => a -> a -> [a]
numericEnumFromTo l h = takeWhile (<= h) (numericEnumFrom l)

numericEnumFromThenTo :: (Num a, Ord a) => a -> a -> a -> [a]
numericEnumFromThenTo l m h =
  if m > l then
    takeWhile (<= h) (numericEnumFromThen l m)
  else
    takeWhile (>= h) (numericEnumFromThen l m)

eftInt :: Int -> Int -> [Int]
eftInt x y = if x `primIntGT` y then [] else go x
  where
    go n = n : if n `primIntEQ` y then [] else go (n `primIntAdd` 1)

efttIntUp :: Int -> Int -> Int -> [Int]
-- x2 >= x1
efttIntUp x1 x2 y
  | y `primIntLT` x2 = if y `primIntLT` x1 then [] else [x1]
  | otherwise =
    let
      delta = x2 `primIntSub` x1
      y' = y `primIntSub` delta
      go x = if x `primIntGT` y' then [x] else x : go (x `primIntAdd` delta)
    in x1 : go x2

efttIntDown :: Int -> Int -> Int -> [Int]
-- x2 <= x1
efttIntDown x1 x2 y
  | y `primIntGT` x2 = if y `primIntGT` x1 then [] else [x1]
  | otherwise =
    let
      delta = x2 `primIntSub` x1
      y' = y `primIntSub` delta
      go x = if x `primIntLT` y' then [x] else x : go (x `primIntAdd` delta)
    in x1 : go x2

-- This instance is difficult to put in Data.Int,
-- so it gets to live here.
instance Enum Int where
  succ x = if x `primIntEQ` maxBound then error "Int.succ: overflow" else x + 1
  pred x = if x `primIntEQ` minBound then error "Int.pred: underflow" else x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom n = eftInt n maxBound
  enumFromThen x1 x2
    | x2 `primIntGE` x1 = efttIntUp x1 x2 maxBound
    | otherwise         = efttIntDown x1 x2 minBound
  enumFromTo = eftInt
  enumFromThenTo x1 x2 y
    | x2 `primIntGE` x1 = efttIntUp x1 x2 y
    | otherwise         = efttIntDown x1 x2 y

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
