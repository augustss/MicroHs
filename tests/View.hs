module View(main) where

f1 :: Int -> Int
f1 ((> 10) -> True) = 1
f1 _ = 0

f2 :: Int -> (Int, Maybe Int, Int)
f2 x@(Just -> y@(Just z)) = (x, y, z)

blah a b = (a, b)

main :: IO ()
main = do
  print (f1 20)
  print (f1 10)
  print (f2 3)
