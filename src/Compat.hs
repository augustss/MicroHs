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
