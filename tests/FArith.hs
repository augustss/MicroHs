module FArith(module FArith) where

import Prelude
import Text.String

list1 :: [Double]
list1 = [-100.343241, -53.3248973, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999]

list2 :: [Double]
list2 = [-100.343241, -53.3248973, 0.0, 1.0, 1.23453523, 3243534.34534, 999.999, 1.2e13]

divide :: Double -> Double -> Double
divide x y = if y == 0.0 then 0.0 else x / y

main :: IO ()
main = do
  putStrLn $ show [ op x y | x <- list1, y <- list2, op <- [(+), (-), (*), divide] ]
  putStrLn $ show [ op x y | x <- list1, y <- list2, op <- [(==), (/=), (<), (<=), (>), (>=)] ]
  putStrLn $ show [ x / y | x <- [2.234983, 1.232, 23.0::Double], y <- [1.0, 5.0, 10.0, 100.0]]
  putStrLn $ show [ x / y | x <- [-2.234983, -1.232, -23.0::Double], y <- [1.0, -5.0, 10.0, -100.0]]
  let str = readDouble "1.576"
  putStrLn $ show str
  putStrLn $ show $ 1.0 + readDouble "2.5"
  putStrLn $ show $ map readDouble ["1.5e32", "1.25e-33"]
