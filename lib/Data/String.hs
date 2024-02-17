module Data.String(
  IsString(..),
  String,
  lines, unlines,
  words, unwords,
  ) where
import Prelude()
import Data.Bool
import Data.Char
import Data.Eq
import Data.Function
import Data.List

class IsString a where
  fromString :: String -> a

lines :: String -> [String]
lines "" = []
lines s =
  case span (not . (== '\n')) s of
    (l, s') ->
      case s' of
        [] -> [l]
        _:s'' -> l : lines s''

unlines :: [String] -> String
unlines = concatMap (++ "\n")

words :: String -> [String]
words s =
  case dropWhile isSpace s of
    "" -> []
    s' -> w : words s''
      where (w, s'') = span (not . isSpace) s'

unwords :: [String] -> String
unwords ss = intercalate " " ss
