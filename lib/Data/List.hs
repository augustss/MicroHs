-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.List(module Data.List) where
import Primitives as P
import Control.Error
import Data.Bool
import Data.Function
import Data.Int
import Data.Maybe
import Data.Tuple

--Yimport Data.Char

--Y{-
infixr 5 :
data [] a = [] | (:) a [a]  -- Parser hacks makes this acceptable
--Y-}

null :: forall a . [a] -> Bool
null [] = True
null _  = False

infixr 5 ++
(++) :: forall a . [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x : xs) ys = x : xs ++ ys 

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

sum :: [P.Int] -> P.Int
sum = foldr (+) 0

product :: [P.Int] -> P.Int
product = foldr (*) 1

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: forall a . (a -> Bool) -> [a] -> Bool
any p = or . map p

all :: forall a . (a -> Bool) -> [a] -> Bool
all p = and . map p

take :: forall a . P.Int -> [a] -> [a]
take n arg =
  if n <= 0 then
    []
  else
    case arg of
      [] -> []
      x : xs -> x : take (n - 1) xs

drop :: forall a . P.Int -> [a] -> [a]
drop n arg =
  if n <= 0 then
    arg
  else
    case arg of
      [] -> []
      _ : xs -> drop (n - 1) xs

length :: forall a . [a] -> P.Int
length =
  -- Make it tail recursive and strict
  let
    rec r [] = r
    rec r (_:xs) =
          let r' = r + 1
          in  r' `primSeq` rec r' xs
  in rec 0

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

stripPrefixBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy eq [] s = Just s
stripPrefixBy eq (c:cs) [] = Nothing
stripPrefixBy eq (c:cs) (d:ds) | eq c d = stripPrefixBy eq cs ds
                               | otherwise = Nothing

isPrefixOfBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
isPrefixOfBy _ [] _ = True
isPrefixOfBy _ _ [] = False
isPrefixOfBy eq (c:cs) (d:ds) = eq c d && isPrefixOfBy eq cs ds

splitAt :: forall a . P.Int -> [a] -> ([a], [a])
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

elemBy :: forall a . (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

enumFrom :: P.Int -> [P.Int]
enumFrom n = n : enumFrom (n+1)

enumFromThen :: P.Int -> P.Int -> [P.Int]
enumFromThen n m = from n
  where d = m - n
        from i = i : from (i+d)

enumFromTo :: P.Int -> P.Int -> [P.Int]
enumFromTo l h = takeWhile (<= h) (enumFrom l)

enumFromThenTo :: P.Int -> P.Int -> P.Int -> [P.Int]
enumFromThenTo l m h =
  if m - l > 0 then
    takeWhile (<= h) (enumFromThen l m)
  else
    takeWhile (>= h) (enumFromThen l m)

find :: forall a . (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs) = if p x then Just x else find p xs

lookupBy :: forall a b . (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x xys = fmapMaybe snd (find (eq x . fst) xys)

unionBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

intersectBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys = filter (\ x -> not (elemBy eq x ys)) xs

deleteBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy eq x (y:ys) = if eq x y then ys else y : deleteBy eq x ys

deleteAllBy :: forall a . (a -> a -> Bool) -> a -> [a] -> [a]
deleteAllBy _ _ [] = []
deleteAllBy eq x (y:ys) = if eq x y then deleteAllBy eq x ys else y : deleteAllBy eq x ys

nubBy :: forall a . (a -> a -> Bool) -> [a] -> [a]
nubBy _ [] = []
nubBy eq (x:xs) = x : nubBy eq (filter (\ y -> not (eq x y)) xs)

replicate :: forall a . P.Int -> a -> [a]
replicate n x = take n (repeat x)

repeat :: forall a . a -> [a]
repeat x =
  let
    xs = x:xs
  in xs

deleteFirstsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq = foldl (flip (deleteBy eq))

deleteAllsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteAllsBy eq = foldl (flip (deleteAllBy eq))

infixl 9 !!
(!!) :: forall a . [a] -> P.Int -> a
(!!) axs i =
  if i < 0 then
    error "!!: <0"
  else
    let
      nth _ [] = error "!!: empty"
      nth n (x:xs) = if n == 0 then x else nth (n - 1) xs
    in nth i axs

eqList :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList _ [] [] = True
eqList eq (x:xs) (y:ys) = eq x y && eqList eq xs ys
eqList _ _ _ = False

partition :: forall a . (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)

-- A simple "quicksort" for now.
sortLE :: forall a . (a -> a -> Bool) -> [a] -> [a]
sortLE _  [] = []
sortLE le (x:xs) =
  case partition (le x) xs of
    (ge, lt) -> sortLE le lt ++ (x : sortLE le ge)

mapMaybe :: forall a b . (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (a:as) =
  case f a of
    Nothing -> mapMaybe f as
    Just b -> b : mapMaybe f as

maybeToList :: forall a . Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

last :: forall a . [a] -> a
last [] = error "last: []"
last [x] = x
last (_:xs) = last xs

init :: forall a . [a] -> [a]
init [] = error "init: []"
init [_] = []
init (x:xs) = x : init xs
