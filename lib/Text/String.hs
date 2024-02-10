-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Text.String(module Text.String) where
import Prelude()              -- do not import Prelude
import Primitives
import Data.Bool
import Data.Char
import Data.Either
import Data.Eq
import Data.Fractional
import Data.Function
import Data.Int
import Data.Integer
import Data.Integral
import Data.List
import Data.Maybe
import Data.Num
import Data.Ord
import Data.Ratio
import Data.Real
import Data.Tuple
import Text.Read
import Text.Show

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

-- Convert string in scientific notation to a rational number.
readRational :: String -> Rational
readRational acs@(sgn:as) | sgn == '-' = negate $ rat1 as
                          | otherwise  =          rat1 acs
  where
    rat1 s1 =
      case span isDigit s1 of
        (ds1, cr1) | ('.':r1) <- cr1                   -> rat2 f1 r1
                   | (c:r1)   <- cr1, toLower c == 'e' -> rat3 f1 r1
                   | otherwise                         -> f1
          where f1 = toRational (read ds1 :: Integer)

    rat2 f1 s2 =
      case span isDigit s2 of
        (ds2, cr2) | (c:r2) <- cr2, toLower c == 'e' -> rat3 f2 r2
                   | otherwise                       -> f2
          where f2 = f1 + toRational (read ds2 :: Integer) * 10 ^^ (negate $ length ds2)

    rat3 f2 ('+':s) = f2 * expo s
    rat3 f2 ('-':s) = f2 / expo s
    rat3 f2      s  = f2 * expo s

    expo s = 10 ^ (read s :: Integer)
