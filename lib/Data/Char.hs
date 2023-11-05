-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Char(
  module Data.Char,
  module Data.Char_Type   -- exports Char and String
  ) where
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
  (<)  = primCharLT
  (<=) = primCharLE
  (>)  = primCharGT
  (>=) = primCharGE

instance Eq String where
  (==) = primStringEQ

instance Ord String where
  compare = primCompare
  x <  y  =  primCompare x y == LT
  x <= y  =  primCompare x y /= GT
  x >  y  =  primCompare x y == GT
  x >=  y =  primCompare x y /= GT

-- ASCII only for now
instance Bounded Char where
  minBound = chr 0
  maxBound = chr 127

chr :: Int -> Char
chr = primChr

ord :: Char -> Int
ord = primOrd

isLower :: Char -> Bool
isLower c = (primCharLE 'a' c) && (primCharLE c 'z')

isUpper :: Char -> Bool
isUpper c = (primCharLE 'A' c) && (primCharLE c 'Z')

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = (primCharLE '0' c) && (primCharLE c '9')

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (primCharLE 'a' c && primCharLE c 'f') || (primCharLE 'F' c && primCharLE c 'F') 

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isPrint :: Char -> Bool
isPrint c = primCharLE ' ' c && primCharLE c '~'

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'

digitToInt :: Char -> Int
digitToInt c | (primCharLE '0' c) && (primCharLE c '9') = ord c - ord '0'
             | (primCharLE 'a' c) && (primCharLE c 'f') = ord c - (ord 'a' - 10)
             | (primCharLE 'A' c) && (primCharLE c 'F') = ord c - (ord 'A' - 10)
             | otherwise                                = error "digitToInt"

toLower :: Char -> Char
toLower c | primCharLE 'A' c && primCharLE c 'Z' = chr (ord c - ord 'A' + ord 'a')
          | True = c

toUpper :: Char -> Char
toUpper c | primCharLE 'a' c && primCharLE c 'a' = chr (ord c - ord 'a' + ord 'A')
          | True = c

instance Show Char where
  showsPrec _ '\'' = showString "'\\''"
  showsPrec _ c = showChar '\'' . showString (encodeChar c) . showChar '\''
  showList    s = showChar '"'  . f s
    where f [] = showChar '"'
          f (c:cs) =
            if c == '"' then showString "\\\"" . f cs
            else showString (encodeChar c) . f cs

-- XXX should not export this
encodeChar :: Char -> String
encodeChar c =
  let
    spec = [('\n', "\\n"), ('\r', "\\r"), ('\t', "\\t"), ('\b', "\\b"),
            ('\\', "\\\\")]
    look [] = if isPrint c then [c] else "'\\" ++ show (ord c) ++ "'"
    look ((d,s):xs) = if d == c then s else look xs
  in look spec
