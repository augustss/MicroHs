-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Integer(
  Integer,
  _intToInteger,
  _integerToInt,
  _wordToInteger,
  _integerToWord,
  _integerToFloatW,
  _integerToRational,
  _integerToIntList,
  _intListToInteger,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Char
import Data.Enum
import Data.Eq
import Data.Function
import Data.Int
import Data.Integer_Type
import Data.Integral
import Data.List
import Data.Num
import Data.Ord
import Data.Ratio_Type
import Data.Real
import Numeric.Show
import Text.Show

--
-- The Integer is stored in sign-magniture format with digits in base maxD (2^31)
-- It has the following invariants:
--  * each digit is >= 0 and < maxD
--  * least signification digits first, most significant last
--  * no trailing 0s in the digits
--  * 0 is positive
{- These definitions are in Integer_Type
data Integer = I Sign [Digit]
  --deriving Show

type Digit = Int

maxD :: Digit
maxD = 2147483648  -- 2^31, this is used so multiplication of two digit doesn't overflow a 64 bit Int

data Sign = Plus | Minus
  --deriving Show
-}

instance Eq Integer where
  (==) = eqI
  (/=) = neI

instance Ord Integer where
  (<)  = ltI
  (<=) = leI
  (>)  = gtI
  (>=) = geI

instance Show Integer where
  showsPrec = showIntegral

{- in Text.Read.Internal
instance Read Integer where
  readsPrec = readIntegral
-}

instance Num Integer where
  (+) = addI
  (-) = subI
  (*) = mulI
  negate = negateI
  abs = absI
  signum x =
    case compare x zeroI of
      LT -> negOneI
      EQ -> zeroI
      GT -> oneI
  fromInteger x = x

instance Integral Integer where
  quotRem = quotRemI
  toInteger x = x

instance Real Integer where
  toRational i = _integerToRational i

instance Enum Integer where
  succ x = x + 1
  pred x = x - 1
  toEnum x = _intToInteger x
  fromEnum x = _integerToInt x
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

------------------------------------------------

isZero :: Integer -> Bool
isZero (I _ ds) = null ds

instance Eq Sign where
  (==) Plus Plus = True
  (==) Minus Minus = True
  (==) _ _ = False

-- Trim off 0s and make an Integer
sI :: Sign -> [Digit] -> Integer
sI s ds =
  case trim0 ds of
    []  -> I Plus []
    ds' -> I s    ds'

zeroD :: Digit
zeroD = 0

addI :: Integer -> Integer -> Integer
addI (I Plus  xs) (I Plus  ys)             =  I Plus  (add xs ys)
addI (I Plus  xs) (I Minus ys) | ltW xs ys = sI Minus (sub ys xs)
                               | True      = sI Plus  (sub xs ys)
addI (I Minus xs) (I Plus  ys) | ltW ys xs = sI Minus (sub xs ys)
                               | True      = sI Plus  (sub ys xs)
addI (I Minus xs) (I Minus ys)             =  I Minus (add xs ys)

negateI :: Integer -> Integer
negateI i@(I _    []) = i
negateI   (I Plus  x) = I Minus x
negateI   (I Minus x) = I Plus  x

absI :: Integer -> Integer
absI (I _ x) = I Plus x

subI :: Integer -> Integer -> Integer
subI x y = addI x (negateI y)

add :: [Digit] -> [Digit] -> [Digit]
add = add' zeroD

add' :: Digit -> [Digit] -> [Digit] -> [Digit]
add' ci (x : xs) (y : ys) = s : add' co xs ys  where (co, s) = addD ci x y
add' ci (x : xs) []       = s : add' co xs []  where (co, s) = addD ci x zeroD
add' ci []       (y : ys) = s : add' co [] ys  where (co, s) = addD ci zeroD y
add' ci []       []       = if ci == zeroD then [] else [ci]

-- Add 3 digits with carry
addD :: Digit -> Digit -> Digit -> (Digit, Digit)
addD x y z = (quot s maxD, rem s maxD)  where s = x + y + z

-- Invariant: xs >= ys, so result is always >= 0
sub :: [Digit] -> [Digit] -> [Digit]
sub xs ys = sub' zeroD xs ys

sub' :: Digit -> [Digit] -> [Digit] -> [Digit]
sub' bi (x : xs) (y : ys) = d : sub' bo xs ys  where (bo, d) = subW bi x y
sub' bi (x : xs) []       = d : sub' bo xs []  where (bo, d) = subW bi x zeroD
sub' 0  []       []       = []
sub' _  []       _        = undefined

-- Subtract with borrow
subW :: Digit -> Digit -> Digit -> (Digit, Digit)
subW b x y =
  let d = x - y + b
  in  if d < 0 then
        (quot d maxD - 1, rem d maxD + maxD)
      else
        (quot d maxD, rem d maxD)

-- Remove trailing 0s
trim0 :: [Digit] -> [Digit]
trim0 = reverse . dropWhile (== (0::Int)) . reverse

-- Is axs < ays?
ltW :: [Digit] -> [Digit] -> Bool
ltW axs ays = lxs < lys || lxs == lys && cmp (reverse axs) (reverse ays)
  where
    lxs = length axs
    lys = length ays
    cmp (x:xs) (y:ys) = x < y || x == y && cmp xs ys
    cmp []     []     = False
    cmp _      _      = error "ltW.cmp"
    
mulI :: Integer -> Integer -> Integer
mulI (I _ []) _ = I Plus []         -- 0 * x = 0
mulI _ (I _ []) = I Plus []         -- x * 0 = 0
mulI (I sx [x]) (I sy ys)  = I (mulSign sx sy) (mulD zeroD ys x)
mulI (I sx xs)  (I sy [y]) = I (mulSign sx sy) (mulD zeroD xs y)
mulI (I sx xs)  (I sy ys)  = I (mulSign sx sy) (mulM xs ys)

mulSign :: Sign -> Sign -> Sign
mulSign s t = if s == t then Plus else Minus

-- Multiply with a single digit, and add carry.
mulD :: Digit -> [Digit] -> Digit -> [Digit]
mulD ci [] _ = if ci == 0 then [] else [ci]
mulD ci (x:xs) y = r : mulD q xs y
  where
    xy = x * y + ci
    q = quot xy maxD
    r = rem  xy maxD

mulM :: [Digit] -> [Digit] -> [Digit]
mulM xs ys =
  let rs = map (mulD zeroD xs) ys
      ss = zipWith (++) (map (`replicate` (0::Int)) [0::Int ..]) rs
  in  foldl1 add ss

-- Signs:
--  + +  -> (+,+)
--  + -  -> (-,+)
--  - +  -> (-,-)
--  - -  -> (+,-)
quotRemI :: Integer -> Integer -> (Integer, Integer)
quotRemI _         (I _  [])  = error "Integer: division by 0" -- n / 0
quotRemI (I _  [])          _ = (I Plus [], I Plus [])         -- 0 / n
quotRemI (I sx xs) (I sy ys) | all (== (0::Int)) ys' =
  -- All but the MSD are 0.  Scale numerator accordingly and divide.
  -- Then add back (the ++) the remainder we scaled off.
    case quotRemD xs' y of
      (q, r) -> qrRes sx sy (q, rs ++ r)
  where ys'       = init ys
        y         = last ys
        n         = length ys'
        (rs, xs') = splitAt n xs  -- xs' is the scaled number
quotRemI (I sx xs) (I sy ys)  = qrRes sx sy (quotRemB xs ys)

qrRes :: Sign -> Sign -> ([Digit], [Digit]) -> (Integer, Integer)
qrRes sx sy (ds, rs) = (sI (mulSign sx sy) ds, sI sx rs)

quotI :: Integer -> Integer -> Integer
quotI x y =
  case quotRemI x y of
    (q, _) -> q

-- Divide by a single digit.
-- Does not return normalized numbers.
quotRemD :: [Digit] -> Digit -> ([Digit], [Digit])
quotRemD axs y = qr zeroD (reverse axs) []
  where
    qr ci []     res = (res, [ci])
    qr ci (x:xs) res = qr r xs (q:res)
      where
        cx = ci * maxD + x
        q = quot cx y
        r = rem cx y

-- Simple iterative long division.
quotRemB :: [Digit] -> [Digit] -> ([Digit], [Digit])
quotRemB xs ys =
  let n  = I Plus xs
      d  = I Plus ys
      a  = I Plus $ replicate (length ys - (1::Int)) (0::Int) ++ [last ys]  -- only MSD of ys
      aq = quotI n a
      ar = addI d oneI
      loop q r =
        if absI r `geI` d then
          let r' = n `subI` (q `mulI` d)
              qn = q `addI` (r' `quotI` a)
              q' = (q `addI` qn) `quotI` twoI
          in  loop q' r'
        else
          q
      q' = loop aq ar
      r = n `subI` (q' `mulI` d)
  in  if r `ltI` zeroI then
        (digits (q' `subI` oneI), digits (r `addI` d))
      else
        (digits q', digits r)

digits :: Integer -> [Digit]
digits (I _ ds) = ds

zeroI :: Integer
zeroI = I Plus []

oneI :: Integer
oneI = I Plus [1]

twoI :: Integer
twoI = I Plus [2]

tenI :: Integer
tenI = I Plus [10]

negOneI :: Integer
negOneI = I Minus [1]

--------------

eqI :: Integer -> Integer -> Bool
eqI (I sx xs) (I sy ys) = sx == sy && xs == ys

neI :: Integer -> Integer -> Bool
neI x y = not (eqI x y)

ltI :: Integer -> Integer -> Bool
ltI (I Plus  xs) (I Plus  ys) = ltW xs ys
ltI (I Minus  _) (I Plus   _) = True
ltI (I Plus   _) (I Minus  _) = False
ltI (I Minus xs) (I Minus ys) = ltW ys xs

leI :: Integer -> Integer -> Bool
leI x y = not (ltI y x)

gtI :: Integer -> Integer -> Bool
gtI x y = ltI y x

geI :: Integer -> Integer -> Bool
geI x y = not (ltI x y)

-- To make the [Int] representing an integer portable, we
-- need to base that does not depend on the word size
integerListBase :: Integer
integerListBase = 32768

-- These two functions return an (opaque) representation of an
-- Integer as [Int].
-- This is used by the compiler to generate Integer literals.
-- First _integerToIntList is used in the compiler to get a list of
-- Int, and the generated code will have a call to _intListToInteger.
_integerToIntList :: Integer -> [Int]
_integerToIntList i = if i < 0 then (-1::Int) : f (-i) else f i
  where f 0 = []
        f i = fromInteger r : f q  where (q, r) = quotRem i integerListBase

_intListToInteger :: [Int] -> Integer
_intListToInteger ads@(x : ds) = if x == -1 then - f ds else f ads
  where f = foldr (\ d a -> a * integerListBase + toInteger d) 0

---------------------------------
{-
pIntegerToInteger :: P.Integer -> Integer
pIntegerToInteger i | i >= 0        = I Plus  (f i)
                    | otherwise     = I Minus (f (negate i))
  where
    f 0 = []
    f x = fromInteger (rem x (toInteger maxD)) : f (quot x (toInteger maxD))

integerToPInteger :: Integer -> P.Integer
integerToPInteger (I s xs) =
  let r = foldr (\ d r -> r * toInteger maxD + toInteger d) 0 xs
  in  case s of
        Plus  -> r
        Minus -> negate r

instance Num Integer where
  (+) = addI
  (-) = subI
  (*) = mulI
  abs x = if x < 0 then -x else x
  signum x = if x > 0 then 1 else if x < 0 then -1 else 0
  fromInteger = pIntegerToInteger

instance Enum Integer where
  fromEnum = fromEnum . integerToPInteger
  toEnum = _intToInteger

instance Real Integer where
  toRational = toRational . toInteger

instance Integral Integer where
  quotRem = quotRemI
  toInteger = integerToPInteger

--instance Show Integer where
--  show = showInteger

instance Eq Integer where
  (==) = eqI

instance Ord Integer where
  x <  y = x `ltI` y
  x <= y = x == y || x `ltI` y
  x >  y = y `ltI` x
  x >= y = x == y || y `ltI` x

instance Arbitrary Integer where
  arbitrary = do
    ndig <- frequency
      [(5,  pure 0)
      ,(25, pure 1)
      ,(20, pure 2)
      ,(10, pure 3)
      ,(10, pure 4)
      ,(2,  pure 5)
      ,(2,  pure 6)
      ]
    digits <- vectorOf ndig (chooseInt (0, maxD - 1))
    sign <- elements [Plus, Minus]
    pure $ if null digits then I Plus [] else I sign digits

{-
newtype SmallInteger = SmallInteger Integer
  deriving Show

instance Arbitrary SmallInteger where
  arbitrary = do
    ndig <- frequency
      [(25, pure 1)
      ,(20, pure 2)
      ,(10, pure 3)
      ,(10, pure 4)
      ]
    digit <- chooseInt (1, maxD - 1)
    sign <- elements [Plus, Minus]
    pure $ SmallInteger $ I sign (replicate (ndig - 1) 0 ++ [digit])
-}
{-
sanity :: HasCallStack => Integer -> Integer
sanity (I Minus []) = undefined
sanity (I _ ds) | any (< 0) ds = undefined
sanity (I _ ds) | length ds > 1 && last ds == 0 = undefined
sanity i = i
-}

prop_roundtrip1 :: Integer -> Bool
prop_roundtrip1 i = fromInteger (toInteger i) == i

prop_negate :: Integer -> Bool
prop_negate i = toInteger (negate i) == negate (toInteger i)

prop_abs :: Integer -> Bool
prop_abs i = toInteger (abs i) == abs (toInteger i)

prop_add :: Integer -> Integer -> Bool
prop_add x y = toInteger (addI x y) == toInteger x + toInteger y

prop_sub :: Integer -> Integer -> Bool
prop_sub x y = toInteger (subI x y) == toInteger x - toInteger y

prop_mul :: Integer -> Integer -> Bool
prop_mul x y = toInteger (mulI x y) == toInteger x * toInteger y

prop_div :: Integer -> NonZero Integer -> Bool
prop_div x (NonZero y) =
  to (quotRemI x y) == toInteger x `quotRem` toInteger y
  where to (a, b) = (toInteger a, toInteger b)
  
prop_muldiv :: Integer -> NonZero Integer -> Bool
prop_muldiv x (NonZero y) =
  let (q, r) = quotRemI x y
  in  q*y + r == x

prop_eq :: Integer -> Integer -> Bool
prop_eq x y = (eqI x y) == (toInteger x == toInteger y)

prop_ne :: Integer -> Integer -> Bool
prop_ne x y = (neI x y) == (toInteger x /= toInteger y)

prop_lt :: Integer -> Integer -> Bool
prop_lt x y = (ltI x y) == (toInteger x < toInteger y)

prop_gt :: Integer -> Integer -> Bool
prop_gt x y = (gtI x y) == (toInteger x > toInteger y)

prop_le :: Integer -> Integer -> Bool
prop_le x y = (leI x y) == (toInteger x <= toInteger y)

prop_ge :: Integer -> Integer -> Bool
prop_ge x y = (geI x y) == (toInteger x >= toInteger y)

prop_show :: Integer -> Bool
prop_show x = showInteger x == show (toInteger x)

checkAll :: IO ()
checkAll = do
  let qc p = quickCheck (withMaxSuccess 100000 p)
  mapM_ qc [prop_roundtrip1, prop_negate, prop_abs, prop_show]
  mapM_ qc [prop_add, prop_sub, prop_mul,
            prop_eq, prop_ne, prop_lt, prop_gt, prop_le, prop_ge]
  mapM_ qc [prop_div, prop_muldiv]
  
-}
