-- Test fake unboxed tuples and sums
module Unboxed where

f# :: Int -> (# Int, Int #)
f# x = (# x , x+1 #)

type T# = (# Int | Bool #)

g :: Int -> T#
g x = (# x | #)

h :: Bool -> Unboxed.T#
h x = (# | x #)

d :: (# Int | Bool #) -> String
d (# x | #) = show x
d (# | x #) = show x

{-
-- Test that regular # still work
(#) :: Int -> Int -> Int
x # y = 2*x - y
-}

main :: IO ()
main = do
  case Unboxed.f# 2 of
    (# x, y #) -> print (x, y)
  putStrLn $ d $ g 2
  putStrLn $ d $ h True
{-
  print $ 3 # 2
  print $ (#) 3 2
  print $ (3 #) 2
  print $ (# 2) 3
-}
