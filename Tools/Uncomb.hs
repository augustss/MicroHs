{-# LANGUAGE DeriveDataTypeable #-}
module Uncomb where
import Data.Char
import Data.Data(Data)
import Data.Generics.Uniplate.Data

infix :@
data Exp
  = S | S' | K | A | U | I | Y | B | B' | Z | C | C' | P | R | O | K2 | K3 | K4 | C'B
  | Exp :@ Exp | Int Integer | Label Int Exp | Ref Int | Vx
  deriving (Show, Read, Data)

reduce :: Exp -> Exp
reduce (((S :@ x) :@ y) :@ z) = (x :@ z) :@ (y :@ z)
reduce ((((S' :@ x) :@ y) :@ z) :@ w) = (x :@ (y :@ w)) :@ (z :@ w)
reduce ((K :@ x) :@ _y) = x
reduce ((A :@ _x) :@ y) = y
reduce ((U :@ x) :@ y) = y :@ x
reduce (I :@ x) = x
reduce (((B :@ x) :@ y) :@ z) = x :@ (y :@ z)
reduce ((((B' :@ x) :@ y) :@ z) :@ w) = (x :@ y) :@ (z :@ w)
reduce (((Z :@ x) :@ y) :@ _z) = (x :@ y)
reduce (((C :@ x) :@ y) :@ z) = (x :@ z) :@ y
reduce ((((C' :@ x) :@ y) :@ z) :@ w) = (x :@ (y :@ w)) :@ z
reduce (((P :@ x) :@ y) :@ z) = (z :@ x) :@ y
reduce (((R :@ x) :@ y) :@ z) = (y :@ z) :@ x
reduce ((((O :@ x) :@ y) :@ z) :@ w) = (w :@ x) :@ y
reduce (((K2 :@ x) :@ _y) :@ _z) = x
reduce ((((K3 :@ x) :@ _y) :@ _z) :@ _w) = x
reduce (((((K4 :@ x) :@ _y) :@ _z) :@ _w) :@ _v) = x
reduce ((((C'B :@ x) :@ y) :@ z) :@ w) = (x :@ z) :@ (y :@ w)
reduce e = e

trans :: String -> String
trace [] = []
trans (' ':cs) = " :@ " ++ trans cs
trans ('#':cs) = "Int " ++ trans cs
trans (':':cs) = "Label " ++ n ++ trans (tail r) where (n,r) = span (/= ' ') cs
trans ('_':cs) = "Ref " ++ n ++ trans r where (n,r) = span isDigit cs
trans (c:cs) = c : trans cs

red :: Exp -> Exp
red = transform reduce

--ttt = (U :@ (K :@ ((B :@ C) :@ (P :@ (Int 111)))))
s1 = "(U (K ((B C) (P #111))))"
s1' = trans s1
t1 = read s1' :: Exp

s2 = "((C ((C (U ((C'B (B' P)) ((B (C' C)) (C P))))) A)) #111)"
s2' = trans s2
t2 = read s2' :: Exp

t1x = (t1 :@ Vx)
t2x = ((t2 :@ Vx))

s3 = "((C B) (C (U (K2 A))))"
s3' = trans s3
t3 = read s3' :: Exp
