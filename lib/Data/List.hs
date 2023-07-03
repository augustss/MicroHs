module Data.List(module Data.List, module Data.List_Type) where
import Control.Error
import Data.Bool
import Data.Function
import Data.Int
import Data.List_Type
import Data.Maybe

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

unzip :: [(a, b)] -> ([a], [b])
unzip axys =
  case axys of
    [] -> ([], [])
    xy : xys ->
      case xy of
        (x, y) ->
          case unzip xys of
            (xs, ys) -> (x:xs, y:ys)

--stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix p s =
  case p of
    [] -> Just s
    c : cs ->
      case s of
        [] -> Nothing
        d : ds ->
          if c == d then
            stripPrefix cs ds
          else
            Nothing

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)
