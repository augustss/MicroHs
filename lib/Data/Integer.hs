-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
-- *** WIP, do not use! ***
module Data.Integer(module Data.Integer) where
import Prelude
{-
import Control.Error
import Data.Bool
import Data.Function
import Data.Int
import Data.List
-}

data Sign = Plus | Minus

type Digit = Int

data Integer = I Sign [Digit]   -- each word is <2^32, least significant digit first, no trailing 0s

intToInteger :: Int -> Integer
intToInteger i | i > 0 = I Plus  (f i)
               | i < 0 = I Minus (f (negate i)) -- XXX minInt
               | True  = I Plus  []
  where f x | x >= maxD = [rem x maxD, quot x maxD]
            | True      = [x]

zeroD :: Digit
zeroD = 0

maxD :: Digit
maxD = 4294967296  -- - 2^32

addI :: Integer -> Integer -> Integer
addI (I Plus  xs) (I Plus  ys)             = I Plus  (add xs ys)
addI (I Plus  xs) (I Minus ys) | ltW xs ys = I Minus (sub ys xs)
                               | True      = I Plus  (sub xs ys)
addI (I Minus xs) (I Plus  ys) | ltW ys xs = I Minus (sub xs ys)
                               | True      = I Plus  (sub ys xs)
addI (I Minus xs) (I Minus ys)             = I Minus (add xs ys)

negateI :: Integer -> Integer
negateI (I Plus  x) = I Minus x
negateI (I Minus x) = I Plus  x

subI :: Integer -> Integer -> Integer
subI x y = addI x (negateI y)

add :: [Digit] -> [Digit] -> [Digit]
add = add' zeroD

add' :: Digit -> [Digit] -> [Digit] -> [Digit]
add' ci (x : xs) (y : ys) = s : add' co xs ys  where (s, co) = addD ci x y
add' ci (x : xs) []       = s : add' co xs []  where (s, co) = addD ci x zeroD
add' ci []       (y : ys) = s : add' co [] ys  where (s, co) = addD ci zeroD y
add' ci []       []       = if ci == zeroD then [] else [ci]

-- Add 3 digits with carry
addD :: Digit -> Digit -> Digit -> (Digit, Digit)
addD x y z = (quot s maxD, rem s maxD)  where s = x + y + z

-- We always have xs >= ys
sub :: [Digit] -> [Digit] -> [Digit]
sub xs ys = trim0 (sub' zeroD xs ys)

sub' :: Digit -> [Digit] -> [Digit] -> [Digit]
sub' bi (x : xs) (y : ys) = d : sub' bo xs ys  where (d, bo) = subW bi x y
sub' bi (x : xs) []       = d : sub' bo xs []  where (d, bo) = subW bi x zeroD
sub' bi []       _        = error "sub'"

subW :: Digit -> Digit -> Digit -> (Digit, Digit)
subW x y z = (quot d maxD, rem d maxD)  where d = y - z + x

-- Remove trailing 0s
trim0 :: [Digit] -> [Digit]
trim0 = reverse . dropWhile (== 0) . reverse

-- Is axs < ays?
ltW :: [Digit] -> [Digit] -> Bool
ltW axs ays = lxs < lys || lxs == lys && cmp (reverse axs) (reverse ays)
  where
    lxs = length axs
    lys = length ays
    cmp (x:xs) (y:ys) = x < y || x == y && cmp xs ys
    cmp []     []     = False
    cmp _      _      = error "cmp"
    
