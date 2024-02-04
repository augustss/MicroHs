-- Copyright 2023,2024 Lennart Augustsson
-- See LICENSE file for full license.
-- Temporary Read class
module Text.Read_Class(
  ReadS,
  Read(..),
  reads,
  readParen,
  ) where
import Primitives
import Data.Bool
import Data.List_Type

type String = [Char]  -- avoid importing Data.Char

type ReadS a = String -> [(a, String)]

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]

  readList = readParen False (\r -> [pr
                                    | ('[',s) <- lex r
                                    , pr <- readl s])
    where readl  s = [([],t)   | (']',t)  <- lex s] ++
                     [(x:xs,u) | (x,t)    <- reads s,
                                 (xs,u)   <- readl' t]
          readl' s = [([],t)   | (']',t)  <- lex s] ++
                     [(x:xs,v) | (',',t)  <- lex s,
                                 (x,u)    <- reads t,
                                 (xs,v)   <- readl' u]

reads :: forall a . Read a => ReadS a
reads = readsPrec 0

readParen :: forall a . Bool -> ReadS a -> ReadS a  
readParen b g =  if b then mandatory else optional  
  where optional r  = g r ++ mandatory r  
        mandatory r = [(x,u) | ('(',s) <- lex r,  
                               (x,t)   <- optional s,  
                               (')',u) <- lex t    ]

-- Really bad lexer
lex :: ReadS Char
lex [] = []
lex (c:cs) | isSpace c = lex cs
           | True = [(c, cs)]

isSpace :: Char -> Bool
isSpace c = c == ' '  || c == '\t' || c == '\n'

infix 4 ==
(==) :: Char -> Char -> Bool
(==) = primCharEQ
