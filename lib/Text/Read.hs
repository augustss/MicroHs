-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
-- Temporary Read class
module Text.Read(
  ReadS,
  Read(..),
  read,
  reads,
  readMaybe,
  readParen,
  ) where
import Primitives
import Control.Error
import Data.Char
import Data.Bool_Type
import Data.Eq
import Data.List_Type
import Data.Maybe_Type
import Data.Num
import Data.Int
import Text.Read_Class

read :: forall a . Read a => String -> a
read s =
  case readMaybe s of
    Just a  -> a
    Nothing -> error "read: failed"

readMaybe :: forall a . Read a => String -> Maybe a
readMaybe s =
  case readsPrec 0 s of
    [(a, ss)] | [] <- skipSpace ss -> Just a
    _ -> Nothing

-- Really bad lexer
lex :: ReadS Char
lex cs =
  case skipSpace cs of
    [] -> []
    c:cs -> [(c, cs)]

skipSpace :: String -> String
skipSpace [] = []
skipSpace ccs@(c:cs) | isSpace c = skipSpace cs
                     | True      = ccs

-------------------------------------------------------
-- To avoid circular imports, some instances go here.

instance Read Int where
  readsPrec _ ccs =
    case lex ccs of
      [('-',cs)]             -> loop True  0 cs
      [(c,  cs)] | isDigit c -> loop False 0 (c:cs)
      _                      -> []
   where
      loop neg res (c:cs) | isDigit c = loop neg (res * (10::Int) + ord c - ord '0') cs
      loop neg res s = [(if neg then -res else res, s)]
