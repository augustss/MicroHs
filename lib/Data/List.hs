-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.List(module Data.List, module Data.List_Type) where
import Control.Error
import Data.Bool
import Data.Function
import Data.Int
import Data.List_Type
import Data.Maybe
import Data.Tuple

-- By parser some parser hacks we can use [] instead of Nil
--data List a = Nil | (:) a (List a)

null :: [a] -> Bool
null arg =
  case arg of
    []    -> True
    _ : _ -> False

(++) :: [a] -> [a] -> [a]
(++) as ys =
  case as of
    [] -> ys
    x : xs -> x : xs ++ ys

concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = concat . map f

map :: (a -> b) -> [a] -> [b]
map f =
  let
    rec arg =
      case arg of
        [] -> []
        a : as -> f a : rec as
  in rec

filter :: (a -> Bool) -> [a] -> [a]
filter p =
  let
    rec arg =
      case arg of
        [] -> []
        x : xs ->
          case p x of
            False -> rec xs
            True  -> x : rec xs
  in rec

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z =
  let
    rec arg =
      case arg of
        [] -> z
        x : xs -> f x (rec xs)
  in rec

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f arg =
  case arg of
    [] -> error "foldr1"
    x : xs -> foldr f x xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z arg =
  case arg of
    [] -> z
    x : xs -> foldl f (f z x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f arg =
  case arg of
    [] -> error "foldl1"
    x : xs -> foldl f x xs

sum = foldr (+) 0
product = foldr (*) 1
and = foldr (&&) True
or = foldr (||) False
any p = or . map p
all p = and . map p

take :: Int -> [a] -> [a]
take n arg =
  case n <= 0 of
    False ->
      case arg of
        [] -> []
        x : xs -> x : take (n-1) xs
    True -> []

drop :: Int -> [a] -> [a]
drop n arg =
  case n <= 0 of
    False ->
      case arg of
        [] -> []
        _ : xs -> drop (n-1) xs
    True -> arg

length :: [a] -> Int
length axs =
  case axs of
    [] -> 0
    _:xs -> 1 + length xs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (\ x y -> (x, y))

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f axs ays =
  case axs of
    [] -> []
    x:xs ->
      case ays of
        [] -> []
        y:ys -> f x y : zipWith f xs ys

unzip :: [(a, b)] -> ([a], [b])
unzip axys =
  case axys of
    [] -> ([], [])
    xy : xys ->
      case xy of
        (x, y) ->
          case unzip xys of
            (xs, ys) -> (x:xs, y:ys)

stripPrefixBy :: (a -> a -> Bool) -> [a] -> [a] -> Maybe [a]
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

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

reverse :: [a] -> [a]
reverse =
  let
    rev r axs =
      case axs of
        [] -> r
        x:xs -> rev (x:r) xs
  in  rev []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p axs =
  case axs of
    [] -> []
    x:xs ->
      if p x then
        x : takeWhile p xs
      else
        []

head :: [a] -> a
head xs =
  case xs of
    [] -> error "head"
    x:_ -> x

tail :: [a] -> [a]
tail xs =
  case xs of
    [] -> error "tail"
    _:ys -> ys

intersperse :: a -> [a] -> [a]
intersperse sep axs =
  case axs of
    [] -> []
    x:xs  -> x : prependToAll sep xs

prependToAll :: a -> [a] -> [a]
prependToAll sep axs =
  case axs of
    [] -> []
    x:xs -> sep : x : prependToAll sep xs

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

enumFrom :: Int -> [Int]
enumFrom n = n : enumFrom (n+1)

find :: (a -> Bool) -> [a] -> Maybe a
find p axs =
  case axs of
    [] -> Nothing
    x:xs ->
      if p x then Just x else find p xs

lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x xys = fmapMaybe snd (find (eq x . fst) xys)

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

deleteBy  :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x ays =
  case ays of
    []   -> []
    y:ys -> if eq x y then ys else y : deleteBy eq x ys

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq axs =
  case axs of
    [] -> []
    x:xs -> x : nubBy eq (filter (\ y -> not (eq x y)) xs)

replicate :: Int -> a -> [a]
replicate n x = take n (repeat x)

repeat :: a -> [a]
repeat x =
  let
    xs = x:xs
  in xs

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq = foldl (flip (deleteBy eq))

(!!) :: Int -> [a] -> a
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

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
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
