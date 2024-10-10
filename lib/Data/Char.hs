-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Char(
  module Data.Char,
  module Data.Char_Type       -- exports Char and String
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Bool
import Data.Bounded
import Data.Char_Type
import Data.Eq
import Data.Function
import Data.Int
import Data.List_Type
import Data.Num
import Data.Ord
import Text.Show

instance Eq Char where
  (==) = primCharEQ
  (/=) = primCharNE

instance Ord Char where
  compare = primCharCompare
  (<)  = primCharLT
  (<=) = primCharLE
  (>)  = primCharGT
  (>=) = primCharGE

-- Using primitive comparison is still a small speedup, even using mostly bytestrings
instance Eq String where
  (==) = primStringEQ

instance Ord String where
  compare =  primStringCompare
  x <  y  =  case primStringCompare x y of { LT -> True; _ -> False }
  x <= y  =  case primStringCompare x y of { GT -> False; _ -> True }
  x >  y  =  case primStringCompare x y of { GT -> True; _ -> False }
  x >= y  =  case primStringCompare x y of { LT -> False; _ -> True }


instance Bounded Char where
  minBound = chr 0
  maxBound = chr 0x10ffff

chr :: Int -> Char
chr = primChr

ord :: Char -> Int
ord = primOrd

isLower :: Char -> Bool
isLower c = (primCharLE 'a' c) && (primCharLE c 'z')

isAsciiLower :: Char -> Bool
isAsciiLower = isLower

isUpper :: Char -> Bool
isUpper c = (primCharLE 'A' c) && (primCharLE c 'Z')

isAsciiUpper :: Char -> Bool
isAsciiUpper = isUpper

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = (primCharLE '0' c) && (primCharLE c '9')

isOctDigit :: Char -> Bool
isOctDigit c = (primCharLE '0' c) && (primCharLE c '7')

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (primCharLE 'a' c && primCharLE c 'f') || (primCharLE 'A' c && primCharLE c 'F') 

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isPrint :: Char -> Bool
isPrint c = primCharLE ' ' c && primCharLE c '~'

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'

isAscii :: Char -> Bool
isAscii c = c <= '\127'

digitToInt :: Char -> Int
digitToInt c | (primCharLE '0' c) && (primCharLE c '9') = ord c - ord '0'
             | (primCharLE 'a' c) && (primCharLE c 'f') = ord c - (ord 'a' - 10)
             | (primCharLE 'A' c) && (primCharLE c 'F') = ord c - (ord 'A' - 10)
             | otherwise                                = error "digitToInt"

intToDigit :: Int -> Char
intToDigit i | i < 10 = chr (ord '0' + i)
             | otherwise = chr (ord 'A' - 10 + i)

toLower :: Char -> Char
toLower c | primCharLE 'A' c && primCharLE c 'Z' = chr (ord c - ord 'A' + ord 'a')
          | True = c

toUpper :: Char -> Char
toUpper c | primCharLE 'a' c && primCharLE c 'a' = chr (ord c - ord 'a' + ord 'A')
          | True = c

instance Show Char where
  showsPrec _ '\'' = showString "'\\''"
  showsPrec _ c = showChar '\'' . showString (encodeChar c "") . showChar '\''
  showList    s = showChar '"'  . f s
    where f [] = showChar '"'
          f (c:cs) =
            if c == '"' then showString "\\\"" . f cs
            else showString (encodeChar c cs) . f cs

-- XXX should not export this
encodeChar :: Char -> String -> String
encodeChar c rest =
  let
    needProtect =
      case rest of
        [] -> False
        c : _ -> isDigit c
    spec = [('\a',"\\a"::String), ('\b', "\\b"::String), ('\f', "\\f"::String), ('\n', "\\n"::String),
            ('\r', "\\r"::String), ('\t', "\\t"::String), ('\v', "\\v"::String), ('\\', "\\\\"::String)]
    look [] = if isPrint c then [c] else ("\\"::String) ++ show (ord c) ++ if needProtect then "\\&"::String else []
    look ((d,s):xs) = if d == c then s else look xs
  in look spec
