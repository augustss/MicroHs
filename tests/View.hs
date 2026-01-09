module View(main) where

f1 :: Int -> Int
f1 ((> 10) -> True) = 1
f1 _ = 0

f2 :: Int -> (Int, Maybe Int, Int)
f2 x@(Just -> y@(Just z)) = (x, y, z)

f3 :: Int -> Int -> Int
f3 x ((== x) -> True) = x
f3 _ _ = 0

f4 :: Int -> Int
f4 x@((==x) -> True) = x

f6 :: Int -> Int -> Int
f6 x (compare x -> LT) = -1
f6 y (compare y -> GT) = 1
f6 _ _ = 0

f7 :: (Int, Int) -> Int
f7 (negate -> x, negate -> y) = x + y

main :: IO ()
main = do
  print (f1 20)
  print (f1 10)
  print (f2 3)
  print (f3 4 4)
  print (f3 4 5)
  print (f4 5)
  print [f6 2 3, f6 3 3, f6 4 3]
  print (f7 (2, 3))
