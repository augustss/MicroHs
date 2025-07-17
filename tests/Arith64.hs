module Arith64 where
import Data.Int
import Data.Word

vals :: [Int64]
vals = [-5, -2, -1, 0, 1, 2, 5]

uvals :: [Word64]
uvals = [-5, -2, -1, 0, 1, 2, 5]

main :: IO ()
main = do
  print [ op x y | x <- vals, y <- vals, op <- [(+),( - ),(*)] ]
  print [ op x y | x <- vals, y <- vals, y /= 0, op <- [quot, rem] ]
  print [ op x y | x <- vals, y <- vals, op <- [(==),(/=),(<),(<=),(>),(>=)] ]
  print [ op x y | x <- vals, y <- vals, let op = compare ]
  print [ minBound, maxBound :: Int64]

  print [ op x y | x <- uvals, y <- uvals, op <- [(+),( - ),(*)] ]
  print [ op x y | x <- uvals, y <- uvals, y /= 0, op <- [quot, rem] ]
  print [ op x y | x <- uvals, y <- uvals, op <- [(==),(/=),(<),(<=),(>),(>=)] ]
  print [ op x y | x <- uvals, y <- uvals, let op = compare ]
  print [ minBound, maxBound :: Word64]
