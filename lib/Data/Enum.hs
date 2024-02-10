module Data.Enum(module Data.Enum) where
import Prelude()              -- do not import Prelude
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

-- This instance is difficult to put in Data.Int,
-- so it gets to live here.
instance Enum Int where
  succ x = x + 1
  pred x = x - 1
  toEnum x = x
  fromEnum x = x
  enumFrom n = n : enumFrom (n+1)
  enumFromThen n m = from n
    where d = m - n
          from i = i : from (i+d)
  enumFromTo l h = takeWhile (<= h) (enumFrom l)
  enumFromThenTo l m h =
    if m > l then
      takeWhile (<= h) (enumFromThen l m)
    else
      takeWhile (>= h) (enumFromThen l m)

-- Likewise for Bool
instance Enum Bool where
  fromEnum False = 0
  fromEnum True  = 1
  toEnum i = if primIntEQ i (0::Int) then False else
             if primIntEQ i (1::Int) then True  else
             error "Enum.Bool.toEnum: bad arg"

instance Enum Char where
  fromEnum = primOrd
  toEnum   = primChr


instance Enum Ordering where
  fromEnum LT = (0::Int)
  fromEnum EQ = (1::Int)
  fromEnum GT = (2::Int)
  toEnum i =      if i `primIntEQ` 0 then LT
             else if i `primIntEQ` 1 then EQ
             else if i `primIntEQ` 2 then GT
             else error "Ord.toEnum: out of range"
