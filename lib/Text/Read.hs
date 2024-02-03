module Text.Read(
  ReadS,
  Read(..),
  read,
  readMaybe,
  readParen,
  ) where
import Primitives
import Control.Error
import Control.Monad
import Data.Char
import Data.Bool_Type
import Data.Eq
import Data.List_Type
import Data.Maybe_Type

type String = [Char]

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]

read :: forall a . Read a => String -> a
read s =
  case readsPrec 0 s of
    [(a, [])] -> a
    _         -> error "read: failed"

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
      -- XXX compiler broken ('(',s) <- lex r
      (lp,s) <- lex r
      guard (lp == '(')
      (x,t) <- optional s
      -- (')',u) <- lex t
      (rp,u) <- lex t
      guard (rp == ')')
      return (x,u)

-- Really bad lexer
lex :: ReadS Char
lex "" = []
lex (c:cs) | isSpace c = lex cs
           | True = [(c, cs)]
