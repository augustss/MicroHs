module Main (main) where

import Prelude

y :: Double
y = -1.37

main :: IO ()
main = putStrLn $ showDouble $ addDouble y y
