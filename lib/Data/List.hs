-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.List(module Data.List) where
import Control.Error
import Data.Bool
import Data.Function
import Data.Int
import Data.Maybe
import Data.Tuple

--Yimport Data.Char
--type Int = P.Int

data [] a = [] | (:) a [a]  -- Parser hacks makes this acceptable --Z

null :: forall a . [a] -> Bool
null [] = True
null _  = False

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
    rec (x : xs) =
          case p x of
            False -> rec xs
            True  -> x : rec xs
  in rec

foldr :: forall a b . (a -> b -> b) -> b -> [a] -> b
foldr f z =
  let
    rec [] = z
    rec (x : xs) = f x (rec xs)
  in rec

foldr1 :: forall a . (a -> a -> a) -> [a] -> a
foldr1 f arg =
  case arg of
    [] -> error "foldr1"
    x : xs -> foldr f x xs

foldl :: forall a b . (b -> a -> b) -> b -> [a] -> b
foldl f z arg =
  case arg of
    [] -> z
    x : xs -> foldl f (f z x) xs

foldl1 :: forall a . (a -> a -> a) -> [a] -> a
foldl1 f arg =
  case arg of
    [] -> error "foldl1"
    x : xs -> foldl f x xs

sum :: [Int] -> Int
sum = foldr (+) 0
product :: [Int] -> Int
product = foldr (*) 1
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
  case n <= 0 of
    False ->
      case arg of
        [] -> []
        x : xs -> x : take (n-1) xs
    True -> []

drop :: forall a . Int -> [a] -> [a]
drop n arg =
  case n <= 0 of
    False ->
      case arg of
        [] -> []
        _ : xs -> drop (n-1) xs
    True -> arg

length :: forall a . [a] -> Int
length axs =
  case axs of
    [] -> 0
    _:xs -> 1 + length xs

zip :: forall a b . [a] -> [b] -> [(a, b)]
zip = zipWith (\ x y -> (x, y))

zipWith :: forall a b c . (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _ = []

unzip :: forall a b . [(a, b)] -> ([a], [b])
unzip axys =
  case axys of
    [] -> ([], [])
    xy : xys ->
      case xy of
        (x, y) ->
          case unzip xys of
            (xs, ys) -> (x:xs, y:ys)

unzip3 :: forall a b c . [(a, b, c)] -> ([a], [b], [c])
unzip3 axyzs =
  case axyzs of
    [] -> ([], [], [])
    xyz : xyzs ->
      case xyz of
        (x, y, z) ->
          case unzip3 xyzs of
            (xs, ys, zs) -> (x:xs, y:ys, z:zs)

stripPrefixBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
stripPrefixBy eq p s =
  case p of
    [] -> Just s
    c : cs ->
      case s of
        [] -> Nothing
        d : ds ->
          if eq c d then
            stripPrefixBy eq cs ds
          else
            Nothing

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
intersperse sep axs =
  case axs of
    [] -> []
    x:xs  -> x : prependToAll sep xs

prependToAll :: forall a . a -> [a] -> [a]
prependToAll sep axs =
  case axs of
    [] -> []
    x:xs -> sep : x : prependToAll sep xs

intercalate :: forall a . [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

elemBy :: forall a . (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

enumFrom :: Int -> [Int]
enumFrom n = n : enumFrom (n+1)

enumFromTo :: Int -> Int -> [Int]
enumFromTo l h = takeWhile (<= h) (enumFrom l)

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
deleteBy eq x ays =
  case ays of
    []   -> []
    y:ys -> if eq x y then ys else y : deleteBy eq x ys

nubBy :: forall a . (a -> a -> Bool) -> [a] -> [a]
nubBy eq axs =
  case axs of
    [] -> []
    x:xs -> x : nubBy eq (filter (\ y -> not (eq x y)) xs)

replicate :: forall a . Int -> a -> [a]
replicate n x = take n (repeat x)

repeat :: forall a . a -> [a]
repeat x =
  let
    xs = x:xs
  in xs

deleteFirstsBy :: forall a . (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq = foldl (flip (deleteBy eq))

(!!) :: forall a . Int -> [a] -> a
(!!) i =
  if i < 0 then
    error "!!: <0"
  else
    let
      nth n axs =
        case axs of
          [] -> error "!!: empty"
          x:xs -> if n == 0 then x else nth (n-1) xs
    in nth i

eqList :: forall a . (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList eq axs ays =
  case axs of
    [] ->
      case ays of
        [] -> True
        _:_ -> False
    x:xs ->
      case ays of
        [] -> False
        y:ys -> eq x y && eqList eq xs ys

partition :: forall a . (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (not . p) xs)
