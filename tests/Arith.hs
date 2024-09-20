module Arith(module Arith) where
import Prelude

vals :: [Int]
vals = [-5, -2, -1, 0, 1, 2, 5]

main :: IO ()
main = do
  putStrLn $ show [ op x y | x <- vals, y <- vals, op <- [(+),( - ),(*)] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, y /= 0, op <- [quot, rem] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, op <- [(==),(/=),(<),(<=),(>),(>=)] ]
  putStrLn $ show [ op x y | x <- vals, y <- vals, let op = compare ]
