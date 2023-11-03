-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.List(
  module Data.List,
  module Data.List_Type
  ) where
import Primitives
import Control.Applicative
import Control.Error
import Control.Monad
import Data.Bool
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int
import Data.List_Type
import Data.Num
import Data.Ord
import Data.Maybe_Type
import Data.Tuple
import Text.Show

--Yimport Data.Char

instance {-# OVERLAPPABLE #-} forall a . Eq a => Eq [a] where
  []     == []      =  True
  (x:xs) == (y:ys)  =  x == y && xs == ys
  _      == _       =  False

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure a = [a]
  (<*>) = ap

instance Monad [] where
  (>>=) = flip concatMap

instance MonadFail [] where
  fail _ = []

instance forall a . Show a => Show [a] where
  showsPrec _ = showList

null :: forall a . [a] -> Bool
null [] = True
null _  = False

concat :: forall a . [[a]] -> [a]
concat = foldr (++) []

concatMap :: forall a b . (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

map :: forall a b . (a -> b) -> [a] -> [b]
map f =
  let
    rec [] = []
    rec (a : as) = f a : rec as
  in rec

filter :: forall a . (a -> Bool) -> [a] -> [a]
filter p =
  let
    rec [] = []
    rec (x : xs) = if p x then x : rec xs else rec xs
  in rec

foldr :: forall a b . (a -> b -> b) -> b -> [a] -> b
foldr f z =
  let
    rec [] = z
    rec (x : xs) = f x (rec xs)
  in rec

foldr1 :: forall a . (a -> a -> a) -> [a] -> a
foldr1 _ [] = error "foldr1"
foldr1 f (x : xs) = foldr f x xs

foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl _ z [] = z
foldl f z (x : xs) = foldl f (f z x) xs

foldl1 :: forall a . (a -> a -> a) -> [a] -> a
foldl1 _ [] = error "foldl1"
foldl1 f (x : xs) = foldl f x xs

minimum :: [Int] -> Int
minimum [] = error "minimum"
minimum (x:ys) = foldr (\ y m -> if y < m then y else m) x ys

sum :: [Int] -> Int
sum = foldr (+) (0::Int)

product :: [Int] -> Int
product = foldr (*) (1::Int)

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: forall a . (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: forall a . (a -> Bool) -> [a] -> Bool
all p = and . map p

take :: forall a . Int -> [a] -> [a]
take n arg =
  if n <= (0::Int) then
    []
  else
    case arg of
      [] -> []
      x : xs -> x : take (n - (1::Int)) xs

drop :: forall a . Int -> [a] -> [a]
drop n arg =
  if n <= (0::Int) then
    arg
  else
    case arg of
      [] -> []
      _ : xs -> drop (n - (1::Int)) xs

length :: forall a . [a] -> Int
length =
  -- Make it tail recursive and strict
  let
    rec r [] = r
    rec r (_:xs) =
          let r' = r + (1::Int)
          in  r' `primSeq` rec r' xs
  in rec (0::Int)

zip :: forall a b . [a] -> [b] -> [(a, b)]
zip = zipWith (\ x y -> (x, y))

zipWith :: forall a b c . (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

-- XXX not as lazy as it could be
unzip :: forall a b . [(a, b)] -> ([a], [b])
unzip axys =
  case axys of
    [] -> ([], [])
    (x,y) : xys ->
      case unzip xys of
        (xs, ys) -> (x:xs, y:ys)

-- XXX not as lazy as it could be
unzip3 :: forall a b c . [(a, b, c)] -> ([a], [b], [c])
unzip3 axyzs =
  case axyzs of
    [] -> ([], [], [])
    (x, y, z) : xyzs ->
      case unzip3 xyzs of
        (xs, ys, zs) -> (x:xs, y:ys, z:zs)

stripPrefix :: forall a . Eq a => [a] -> [a] -> Maybe [a]
stripPrefix = stripPrefixBy (==)

stripPrefixBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy eq [] s = Just s
stripPrefixBy eq (c:cs) [] = Nothing
stripPrefixBy eq (c:cs) (d:ds) | eq c d = stripPrefixBy eq cs ds
                               | otherwise = Nothing

isPrefixOf :: forall a . Eq a => [a] -> [a] -> Bool
isPrefixOf = isPrefixOfBy (==)

isPrefixOfBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
isPrefixOfBy eq (c:cs) (d:ds) = eq c d && isPrefixOfBy eq cs ds
isPrefixOfBy _ [] _ = True
isPrefixOfBy _ _  _ = False

splitAt :: forall a . Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

reverse :: forall a . [a] -> [a]
reverse =
  let
    rev r [] = r
    rev r (x:xs) = rev (x:r) xs
  in  rev []

takeWhile :: forall a . (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) =
  if p x then
    x : takeWhile p xs
  else
    []

dropWhile :: forall a . (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) =
  if p x then
    dropWhile p xs
  else
    x : xs

span :: forall a . (a -> Bool) -> [a] -> ([a], [a])
span p =
  let
    rec r [] = (reverse r, [])
    rec r (x:xs) = if p x then rec (x:r) xs else (reverse r, x:xs)
  in rec []

spanUntil :: forall a . (a -> Bool) -> [a] -> ([a], [a])
spanUntil p =
  let
    rec r [] = (reverse r, [])
    rec r (x:xs) = if p x then rec (x:r) xs else (reverse (x:r), xs)
  in rec []

head :: forall a . [a] -> a
head [] = error "head"
head (x:_) = x

tail :: forall a . [a] -> [a]
tail [] = error "tail"
tail (_:ys) = ys

intersperse :: forall a . a -> [a] -> [a]
intersperse _ [] = []
intersperse sep (a:as) = a : prepend as
  where
    prepend [] = []
    prepend (x:xs) = sep : x : prepend xs

intercalate :: forall a . [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

elem :: forall a . (Eq a) => a -> [a] -> Bool
elem = elemBy (==)

notElem :: forall a . (Eq a) => a -> [a] -> Bool
notElem a as = not (elem a as)

elemBy :: forall a . (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

enumFrom :: Int -> [Int]
enumFrom n = n : enumFrom (n+1)

enumFromThen :: Int -> Int -> [Int]
enumFromThen n m = from n
  where d = m - n
        from i = i : from (i+d)

enumFromTo :: Int -> Int -> [Int]
enumFromTo l h = takeWhile (<= h) (enumFrom l)

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo l m h =
  if m > l then
    takeWhile (<= h) (enumFromThen l m)
  else
    takeWhile (>= h) (enumFromThen l m)

find :: forall a . (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

lookup :: forall a b . Eq a => a -> [(a, b)] -> Maybe b
lookup = lookupBy (==)

lookupBy :: forall a b . (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x xys =
  case find (eq x . fst) xys of
    Nothing -> Nothing
    Just (_, b) -> Just b

union :: forall a . Eq a => [a] -> [a] -> [a]
union = unionBy (==)

unionBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

intersect :: forall a . Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersectBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys = filter (\ x -> not (elemBy eq x ys)) xs

deleteBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y:ys) = if eq x y then ys else y : deleteBy eq x ys

deleteAllBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteAllBy _ _ [] = []
deleteAllBy eq x (y:ys) = if eq x y then deleteAllBy eq x ys else y : deleteAllBy eq x ys

nub :: forall a . Eq a => [a] -> [a]
nub = nubBy (==)

nubBy :: forall a . (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\ y -> not (eq x y)) xs)

replicate :: forall a . Int -> a -> [a]
replicate n x = take n (repeat x)

repeat :: forall a . a -> [a]
repeat x =
  let
    xs = x:xs
  in xs

infix 5 \\
(\\) :: forall a . Eq a => [a] -> [a] -> [a]
(\\) = deleteFirstsBy (==)

deleteFirstsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq = foldl (flip (deleteBy eq))

-- Delete all from the second argument from the first argument
deleteAllsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteAllsBy eq = foldl (flip (deleteAllBy eq))

infixl 9 !!
(!!) :: forall a . [a] -> Int -> a
(!!) axs i =
  if i < (0::Int) then
    error "!!: <0"
  else
    let
      nth _ [] = error "!!: empty"
      nth n (x:xs) = if n == (0::Int) then x else nth (n - (1::Int)) xs
    in nth i axs

partition :: forall a . (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- A simple "quicksort" for now.
sortLE :: forall a . (a -> a -> Bool) -> [a] -> [a]
sortLE _  [] = []
sortLE le (x:xs) =
  case partition (le x) xs of
    (ge, lt) -> sortLE le lt ++ (x : sortLE le ge)

last :: forall a . [a] -> a
last [] = error "last: []"
last [x] = x
last (_:xs) = last xs

init :: forall a . [a] -> [a]
init [] = error "init: []"
init [_] = []
init (x:xs) = x : init xs

anySame :: forall a . Eq a => [a] -> Bool
anySame = anySameBy (==)

anySameBy :: forall a . (a -> a -> Bool) -> [a] -> Bool
anySameBy _ [] = False
anySameBy eq (x:xs) = elemBy eq x xs || anySameBy eq xs

iterate :: forall a . (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
