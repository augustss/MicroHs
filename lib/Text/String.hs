-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Text.String(module Text.String) where
import Primitives
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import Data.Num
import Data.Ord
import Data.Tuple
import Text.Show

xshowChar :: Char -> String
xshowChar c = "'" ++ xencodeChar c ++ "'"

xencodeChar :: Char -> String
xencodeChar c =
  let
    spec = [('\n', "\\n"), ('\r', "\\r"), ('\t', "\\t"), ('\b', "\\b"),
            ('\\', "\\\\"), ('\'', "\\'"), ('"', "\"")]
  in
    case lookup c spec of
      Nothing -> if isPrint c then [c] else "'\\" ++ show (ord c) ++ "'"
      Just s  -> s

readInt :: String -> Int
readInt cs =
  let
    rd = foldl (\ a c -> a * 10 + ord c - ord '0') 0
  in if head cs == '-' then 0 - rd (tail cs) else rd cs

readDouble :: String -> Double
readDouble = primDoubleRead

showListS :: forall a . (a -> String) -> [a] -> String
showListS sa as = showListWith (\ a s -> sa a ++ s) as ""

showPairS :: forall a b . (a -> String) -> (b -> String) -> (a, b) -> String
showPairS sa sb (a, b) = "(" ++ sa a ++ "," ++ sb b ++ ")"

lines :: String -> [String]
lines "" = []
lines s =
  case span (not . (== '\n')) s of
    (l, s') -> case s' of { [] -> [l]; _:s'' -> l : lines s'' }

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

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

forceString :: String -> ()
forceString [] = ()
forceString (c:cs) = c `primSeq` forceString cs

compareString :: String -> String -> Ordering
compareString = primCompare

