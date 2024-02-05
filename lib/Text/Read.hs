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
import Primitives
import Control.Error
import Data.Char
import Data.Bool
import Data.Eq
import Data.Function
import Data.List
import Data.Maybe_Type
import Data.Num
import Data.Int
import Text.Read.Lex

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]

  readList = readParen False (\r -> [pr
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

readParen :: forall a . Bool -> ReadS a -> ReadS a  
readParen b g =  if b then mandatory else optional  
  where optional r  = g r ++ mandatory r  
        mandatory r = [(x,u) | ("(",s) <- lex r,  
                               (x,t)   <- optional s,  
                               (")",u) <- lex t ]

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
  readsPrec _ = readParen False $ \ s ->
    [ ( i, t) |                   (cs@(c:_), t) <- lex s, isDigit c, i <- loop 0 cs] ++
    [ (-i, t) | ("-",r) <- lex s, (cs@(c:_), t) <- lex r, isDigit c, i <- loop 0 cs]
   where
      loop res (c:cs) | isDigit c = loop (res * (10::Int) + ord c - ord '0') cs
      loop res [] = [res]
      loop _ _ = []

instance forall a . Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ s = [ (False, r) | ("False", r) <- lex s ] ++
                  [ (True,  r) | ("True",  r) <- lex s ]
