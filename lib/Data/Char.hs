-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module Data.Char(module Data.Char) where
import qualified Primitives as P
import Data.Bool
import Data.Int

--Ytype Char = P.Char
--Ytype Int  = P.Int
type String = [Char]

chr :: Int -> Char
chr = P.primChr

ord :: Char -> Int
ord = P.primOrd

isLower :: Char -> Bool
isLower c = (P.primCharLE 'a' c) && (P.primCharLE c 'z')

isUpper :: Char -> Bool
isUpper c = (P.primCharLE 'A' c) && (P.primCharLE c 'Z')

isAlpha :: Char -> Bool
isAlpha c = isLower c || isUpper c

isDigit :: Char -> Bool
isDigit c = (P.primCharLE '0' c) && (P.primCharLE c '9')

isPrint :: Char -> Bool
isPrint c = P.primCharLE ' ' c && P.primCharLE c '~'

eqChar :: Char -> Char -> Bool
eqChar = P.primCharEQ

neChar :: Char -> Char -> Bool
neChar = P.primCharNE

leChar :: Char -> Char -> Bool
leChar = P.primCharLE

ltChar :: Char -> Char -> Bool
ltChar = P.primCharLT
