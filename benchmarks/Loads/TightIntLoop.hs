module Loads.TightIntLoop(main) where

-- Strict tail-recursive Int accumulation. Stresses basic arithmetic
-- dispatch and the boxing/unboxing path for Int, with essentially no
-- allocation once the loop is running (presumably).
sumTo :: Int -> Int -> Int
sumTo !acc 0 = acc
sumTo !acc n = sumTo (acc + n) (n - 1)

main :: IO ()
main = do
  let n = 5000000
  print (sumTo 0 n)
