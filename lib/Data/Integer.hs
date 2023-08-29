-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
-- *** WIP, do not use! ***
module Data.Integer(Integer) where
--Yimport Primitives(Word)
import Control.Error
import Data.Bool
import Data.Function
import qualified Data.Int as I
import Data.List
import qualified Data.Word as W

data Sign = Plus | Minus

data Integer = I Sign [Word]   -- each word is <2^32, LSW first

zeroW :: Word
zeroW = W.intToWord 0

maxW :: Word
maxW = W.intToWord 4294967296

(+) :: Integer -> Integer -> Integer
(+) (I Plus  xs) (I Plus  ys)             = I Plus    (add xs ys)
(+) (I Plus  xs) (I Minus ys) | ltW xs ys = I Minus (sub ys xs)
                              | True      = I Plus  (sub xs ys)
(+) (I Minus xs) (I Plus  ys) | ltW ys xs = I Minus (sub xs ys)
                              | True      = I Plus  (sub ys xs)
(+) (I Minus xs) (I Minus ys)             = I Minus   (add xs ys)

negate :: Integer -> Integer
negate (I Plus  x) = I Minus x
negate (I Minus x) = I Plus  x

(-) :: Integer -> Integer -> Integer
(-) x y = x + negate y

add :: [Word] -> [Word] -> [Word]
add = add' zeroW

add' :: Word -> [Word] -> [Word] -> [Word]
add' ci (x : xs) (y : ys) = s : add' co xs ys  where (s, co) = addW ci x y
add' ci (x : xs) []       = s : add' co xs []  where (s, co) = addW ci x zeroW
add' ci []       (y : ys) = s : add' co [] ys  where (s, co) = addW ci zeroW y
add' ci []       []       = if (W.==) ci zeroW then [] else [ci]

-- Add 3 words with carry
addW :: Word -> Word -> Word -> (Word, Word)
addW x y z = (W.quot s maxW, W.rem s maxW)  where s = (W.+) ((W.+) x y) z

-- We always have xs <= ys
sub :: [Word] -> [Word] -> (Sign, [Word])
sub xs ys = if ltW xs ys then (Minus, sub' zeroW ys xs) else (Plus, sub' zeroW xs ys)

sub' :: Word -> [Word] -> [Word] -> [Word]
sub' bi (x : xs) (y : ys) = d : sub' bo xs ys  where (d, bo) = subW bi x y

subW :: Word -> Word -> Word -> (Word, Word)
subW _ _ _ = error "subW"

ltW :: [Word] -> [Word] -> Bool
ltW axs ays = (I.<) lxs lys || (I.==) lxs lys && cmp (reverse axs) (reverse ays)
  where
    lxs = length axs
    lys = length ays
    cmp (x:xs) (y:ys) = (W.<) x y || (W.==) x y && cmp xs ys
    cmp []     []     = False
    cmp _      _      = error "cmp"
    
