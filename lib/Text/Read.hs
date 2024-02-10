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
  lex,
  ) where
import Prelude()              -- do not import Prelude
import Primitives
import Control.Error
import Data.Char
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Ord
import Data.Int
import Text.Read.Numeric
import Text.Read.Lex

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]

  readList = readParen False (\r -> [ pr
                                    | ("[",s) <- lex r
                                    , pr <- readl s])
    where readl  s = [([],t)   | ("]",t)  <- lex s] ++
                     [(x:xs,u) | (x,t)    <- reads s,
                                 (xs,u)   <- readl' t]
          readl' s = [([],t)   | ("]",t)  <- lex s] ++
                     [(x:xs,v) | (",",t)  <- lex s,
                                 (x,u)    <- reads t,
                                 (xs,v)   <- readl' u]

reads :: forall a . Read a => ReadS a
reads = readsPrec 0

read :: forall a . Read a => String -> a
read s =
  case readMaybe s of
    Just a  -> a
    Nothing -> error "read: failed"

readMaybe :: forall a . Read a => String -> Maybe a
readMaybe s =
  case readsPrec 0 s of
    [(a, ss)] | [] <- dropSpace ss -> Just a
    _ -> Nothing

-------------------------------------------------------
-- To avoid circular imports, some instances go here.

instance Read Int where
  readsPrec = readIntegral

instance forall a . Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ = readBoundedEnum

instance Read Ordering where
  readsPrec _ = readBoundedEnum

instance forall a . Read a => Read (Maybe a) where
  readsPrec p u = [ (Nothing :: Maybe a, t) | ("Nothing", t) <- lex u ] ++
                  readParen (p > 10) ( \ r ->
                    [ (Just a,  t) | ("Just",    s) <- lex r, (a, t) <- readsPrec 11 s ]
                    ) u

instance forall a b . (Read a, Read b) => Read (Either a b) where
  readsPrec p = readParen (p > 10) $ \ r ->
                [ (Left  a, t) | ("Left",  s) <- lex r, (a, t) <- readsPrec 11 s ] ++
                [ (Right b, t) | ("Right", s) <- lex r, (b, t) <- readsPrec 11 s ]

instance Read () where
  readsPrec p  = readParen False $
                   \ r -> [((),t) | ("(",s) <- lex r,
                                    (")",t) <- lex s ]

instance forall a b . (Read a, Read b) => Read (a,b)  where
  readsPrec p = readParen True $
                  \ r -> [((a, b), u) | (a, s)   <- reads r,
                                        (",", t) <- lex s,
                                        (b, u)   <- reads t ]

instance Read Char where
    readsPrec p = readParen False $
                    \ r -> [(c,t) | ('\'':s,t)<- lex r,
                                    (c,"\'")  <- readLitChar s]
 
    readList = readParen False $ \ r -> [(l,t) | ('"':s, t) <- lex r,
                                                 (l,_)      <- readl s ]
        where readl ('"':s)      = [("",s)]
              readl ('\\':'&':s) = readl s
              readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
                                               (cs,u) <- readl t ]

readLitChar :: ReadS Char
readLitChar ('\\':s) = readEsc s
readLitChar (c:s)    = [(c, s)]

readEsc :: ReadS Char
readEsc ('a':s)  = [('\a',s)]
readEsc ('b':s)  = [('\b',s)]
readEsc ('f':s)  = [('\f',s)]
readEsc ('n':s)  = [('\n',s)]
readEsc ('r':s)  = [('\r',s)]
readEsc ('t':s)  = [('\t',s)]
readEsc ('v':s)  = [('\v',s)]
readEsc ('\\':s) = [('\\',s)]
readEsc ('"':s)  = [('"',s)]
readEsc ('\'':s) = [('\'',s)]
readEsc ('^':c:s) | c >= '@' && c <= '_'
                 = [(chr (ord c - ord '@'), s)]
readEsc s@(d:_) | isDigit d
                 = [(chr n, t) | (n,t) <- readDec s]
readEsc ('o':s)  = [(chr n, t) | (n,t) <- readOct s]
readEsc ('x':s)  = [(chr n, t) | (n,t) <- readHex s]
readEsc _        = []
