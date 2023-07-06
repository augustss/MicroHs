module Compat(module Compat) where
import Data.List

-- Functions needed for ghc
eqChar :: Char -> Char -> Bool
eqChar = (==)

neChar :: Char -> Char -> Bool
neChar = (/=)

eqString :: String -> String -> Bool
eqString = (==)

readInt :: String -> Int
readInt = read

showInt :: Int -> String
showInt = show

showString :: String -> String
showString = show

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq a = any (eq a)

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

lookupBy :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x xys = fmap snd (find (eq x . fst) xys)

pair :: a -> b -> (a, b)
pair = (,)

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

eqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> (a, b) -> (a, b) -> Bool
eqPair eqa eqb ab1 ab2 =
  case ab1 of
    (a1, b1) ->
      case ab2 of
        (a2, b2) ->
          eqa a1 a2 && eqb b1 b2

eqInt :: Int -> Int -> Bool
eqInt = (==)
