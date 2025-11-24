module View(main) where

f :: Int -> Int
f ((> 10) -> True) = 1
f _ = 0

g :: Int -> Int -> Int
g x ((==x) -> True) = x
g _ _ = 0

main :: IO ()
main = do
  print (f 20)
  print (f 10)
  print (g 2 2)
  print (g 2 1)
