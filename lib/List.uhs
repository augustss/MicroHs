module List where
import Prelude

-- By parser some parser hacks we can use [] instead of Nil
data List a = Nil | (:) a (List a)

(++) :: [a] -> [a] -> [a]
(++) as ys =
  case as of
    [] -> ys
    x : xs -> x : xs ++ ys

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
