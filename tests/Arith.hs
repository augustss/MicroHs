module Arith(module Arith) where
import Prelude

main :: IO ()
main = do
  putStrLn $ show [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5]::[Int], y <- [0 - 5,0 - 2,0 - 1,0,1,2,5::Int], op <- [(+),( - ),(*)] ]
  putStrLn $ show [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5]::[Int], y <- [0 - 5,0 - 2,0 - 1,1,2,5::Int], op <- [quot, rem] ]
  putStrLn $ show [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5]::[Int], y <- [0 - 5,0 - 2,0 - 1,0,1,2,5::Int], op <- [(==),(/=),(<),(<=),(>),(>=)] ]
