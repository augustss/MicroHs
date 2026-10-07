module Loads.NumericIntegration(main) where

-- Strict tail-recursive Double accumulation in a tight loop. Same shape as
-- TightIntLoop, but over Double, so it isolates the floating-point
-- add/box path instead of Int arithmetic.
sumTo :: Double -> Int -> Double
sumTo !acc 0 = acc
sumTo !acc n = sumTo (acc + fromIntegral n) (n - 1)

main :: IO ()
main = do
  let n = 5000000
  print (sumTo 0.0 n)
