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
import Control.Monad
import Control.Monad.Fail
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
  case readsPrec 0 s of
    [(a, [])] -> a
    _         -> error "read: failed"

reads :: forall a . Read a => ReadS a
reads = readsPrec 0

readMaybe :: forall a . Read a => String -> Maybe a
readMaybe s =
  case readsPrec 0 s of
    [(a, [])] -> Just a
    _         -> Nothing

readParen :: forall a . Bool -> ReadS a -> ReadS a
readParen b g =
  if b then mandatory else optional
  where
    optional :: ReadS a
    optional r = g r ++ mandatory r
    mandatory :: ReadS a
    mandatory r = do
      ('(',s) <- lex r
      (x,t) <- optional s
      (')',u) <- lex t
      return (x,u)

-- Really bad lexer
lex :: ReadS Char
lex "" = []
lex (c:cs) | isSpace c = lex cs
           | True = [(c, cs)]

-------------------------------------------------------
-- To avoid circular imports, some instances go here.

-- XXX make this better
instance Read Int where
  readsPrec _ ccs@(c:cs) | isDigit c = loop False 0 ccs
    where
      loop neg res (c:cs) | isDigit c = loop neg (res * (10::Int) + ord c - ord '0') cs
      loop neg res s = [(if neg then -res else res, s)]
  readsPrec _ _ = []
