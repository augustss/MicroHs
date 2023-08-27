module Arith(module Arith) where
import Prelude

main :: IO ()
main = do
  putStrLn $ showList showInt [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5], y <- [0 - 5,0 - 2,0 - 1,0,1,2,5], op <- [(+),( - ),(*)] ]
  putStrLn $ showList showInt [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5], y <- [0 - 5,0 - 2,0 - 1,1,2,5], op <- [quot, rem] ]
  putStrLn $ showList showBool [ op x y | x <- [0 - 5,0 - 2,0 - 1,0,1,2,5], y <- [0 - 5,0 - 2,0 - 1,0,1,2,5], op <- [(==),(/=),(<),(<=),(>),(>=)] ]
