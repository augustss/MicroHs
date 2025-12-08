module Logarithms where

import Data.Foldable (for_)
import Math.NumberTheory.Logarithms

main :: IO ()
main = do
    -- integerLogBase
    for_ [2, 3, 4, 5, 10, 20, 40] $ \b -> do
        for_ [1..42] $ \k -> do
            print $ integerLogBase b (b ^ k - 1)
            print $ integerLogBase b (b ^ k)
            print $ integerLogBase b (b ^ k + 1)

    -- integerLog2
    for_ [1..42] $ \k -> do
        print $ integerLog2 (2 ^ k - 1)
        print $ integerLog2 (2 ^ k)
        print $ integerLog2 (2 ^ k + 1)

    -- integerLog10
    for_ [1..42] $ \k -> do
        print $ integerLog10 (10 ^ k - 1)
        print $ integerLog10 (10 ^ k)
        print $ integerLog10 (10 ^ k + 1)
