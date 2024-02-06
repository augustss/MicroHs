module ListCompr(main) where
import Prelude
default (Int)

main :: IO ()
main = do
  print [ x | x <- [1..3] ]
  print [ x | x <- [1..3], odd x ]
  print [ x+1 | x <- [1..3] ]
  print [ (x,y) | x <- [1..3], y <- [1,2] ]
  print $ [ x | x <- [1..3] ] ++ [ x | x <- [1..4] ]
  print [ [ x + y | y <- [1,2] ] | x <- [1..3] ]
  print [ x+1 | x <- [ a+b | a <- [1,10,100], b <- [2,3] ], even x ]
